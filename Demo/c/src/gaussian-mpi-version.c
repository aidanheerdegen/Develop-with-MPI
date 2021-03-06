#include "gaussian-mpi.h"
extern input* parms;
void* check_malloc(size_t bytes, char* msg){
	void* p = malloc (bytes);
	if (p == NULL)
		printf("%s\n",msg);
	return (p);
}
void check_flag(int flag, int error){
	if (flag != MPI_SUCCESS){
		printf(" MPI call failed with error %d\n",error);
		MPI_Abort(MPI_COMM_WORLD,error);
	}
}


void Read_command_line_arguments(int* argc,char** argv, input* parms){
	int i;
	if (*argc > 1){
		for (i=0; i< *argc; i++){
			if ( argv[i][0] == '-'){
				if (argv[i][1] == 'n'){
					parms->neq = atoi(argv[i+1]);
					parms->num_row=parms->neq;
					parms->num_col=parms->neq+1;
				}
				else if (argv[i][1] == 'f'){
					parms->filename=(char*) malloc (256*sizeof(char));
					sprintf(parms->filename,"%s",argv[i+1]);
				}
			}
		}    
	}
}

float** allocate_mat(int rows, int cols){
	float** A; 
	A = (float**) check_malloc(rows * sizeof(float*),"Allocation of augmented matrix failed");
	for (int i =0; i<rows;i++){
		A[i] = (float*) check_malloc( cols * sizeof(float),"Allocation of augmented matrix failed");
	}
	for (int i=0; i<rows; i++){
		for (int j=0; j<cols; j++){
			A[i][j] = 0.0;
		}
	}
	return A;
}



void set_to_default_system(float** A){
	A[0][0] = 3.0;  A[0][1] = 1.0;  A[0][2] = 9.0;   A[0][3] = 5.0;  A[0][4] = 1.0;

	A[1][0] = 3.0;  A[1][1] = 5.0;  A[1][2] = 3.0;   A[1][3] = 4.0/3.0;  A[1][4] = 4.0;

	A[2][0] = 4.0/3.0;  A[2][1] = 4.0/3.0;  A[2][2] = 2.0;   A[2][3] = 3.0;  A[2][4] = 9.0/4.0;

	A[3][0] = 4.0/3.0;  A[3][1] = 1.0/2.0;  A[3][2] = 5.0/3.0;   A[3][3] = 7.0;  A[3][4] = 8.0;

}



void read_in_system(int neq, float** A,char* filename){
	FILE* f= fopen(filename,"r");
	for (int i =0; i<neq; i++){
		for (int j=0; j<neq+1; j++){
			fscanf(f,"%f",&A[i][j]);
		}
	}
	fclose(f);
}


void write_out(int neq , float** A){
	for (int i =0; i<neq; i++){
		for (int j=0;j<neq+1; j++){
			printf("%-3.5f   ",A[i][j]);
		}
		printf("\n");
	}
}

// Evaluate the solution for every row starting from the last going up to first
// Food for thought: 
// Can this function be parallised with MPI
float* backward_substitution(int N, float** A){
	
        float* x= (float*) check_malloc(N*sizeof(float),"Allocation failed for Solution vector in function solve."); 
        for (int i=N-1; i>=0; i--){
                x[i] = A[i][N];
                for (int j=(N-1); j>i; j--){
                        x[i] -= (A[i][j] * x[j]);
                }
                x[i] = x[i]/A[i][i];
        }

        return x;

}

/* Estimate the division of columns amongst MPI-tasks. If the columns
 * are not divisible by the number of total MPI-tasks, then the
 * last MPI-tasks gets the larger chunk with the its own + the 
 * remainder
 */ 
void adjust_chunk_size(int n_columns, int num_tasks, int* chunk,int* col_start_indx){
	int rem = n_columns % num_tasks;
	int uniform_chunk = (n_columns-rem)/num_tasks; 
	for (int i =0 ; i < num_tasks; i++){
		if ( i == num_tasks - 1){
			chunk[i] = uniform_chunk + rem;
			col_start_indx[i] = i*uniform_chunk;
		}
		else{
			chunk[i] = uniform_chunk;
			col_start_indx[i] = i*uniform_chunk;
		}
	}
} 

// MASTER decided the workload and distributes amongst worker
void distribute_columns(int rank, int num_tasks, float** A){
	int *chunk;
	MPI_Request send_req, recv_req;
	parms->col_start_indx = check_malloc(num_tasks*sizeof(int), "Failed to allocated matrix for chunk size");

	// MASTER decides chunk size and Scatters the correponding count of columns the worker processors
	// The first argument of function "adjust_chunk_size" is neq because we are distributing an augmented matrix
	if (rank == MASTER){ 
		chunk = (int*) check_malloc(num_tasks*sizeof(int), "Failed to allocated matrix for chunk size");
		adjust_chunk_size(parms->num_col, num_tasks, chunk, parms->col_start_indx);
		for (int i =0; i<num_tasks; i++)
			printf("%d ",chunk[i]);
		printf("\n");
	}
	check_flag(MPI_Scatter(chunk,1,MPI_INT,&parms->num_my_col,1, MPI_INT, MASTER, MPI_COMM_WORLD),3);	
	check_flag(MPI_Bcast(parms->col_start_indx,num_tasks,MPI_INT,MASTER,MPI_COMM_WORLD),4);
	//Allocate the placeholder for the columns to recieve by the MASTER
	parms->my_col=  allocate_mat(parms->num_row,parms->num_my_col);
	float recv[parms->num_row*parms->num_my_col];
	float* sendbuf;
	int sendcount[num_tasks],disp[num_tasks];
	if (rank == MASTER){
		int count=parms->num_row*parms->num_col;
		sendbuf = (float*) check_malloc(count*sizeof(float),"Failed to allocated sendbuf in distribute_colums");
		int tmp_disp=0;
		for (int i=0; i< num_tasks; i++){
			sendcount[i] = parms->num_row * chunk[i];
			disp[i] = tmp_disp;
			tmp_disp+=parms->num_row * chunk[i];
		}
		for (int col=0; col<parms->num_col; col++){
			for (int row=0; row<parms->num_row; row++){
				sendbuf[col*parms->num_row + row] = A[row][col];
			}
		}
	}
	check_flag(MPI_Scatterv(sendbuf,sendcount,disp,MPI_FLOAT,recv,parms->num_my_col*parms->num_row,MPI_FLOAT,MASTER,MPI_COMM_WORLD),5);
	for (int j=0; j<parms->num_my_col; j++){
		for (int i=0; i< parms->num_row; i++){
			parms->my_col[i][j] = recv[j*parms->num_row + i];
		}
	}

	if (rank ==MASTER) free(sendbuf);

}

//Solves the work allocated by MASTER
void solve_mpi(int rank, int num_tasks){
	//Root MPI-task broadcasting the pivots
	int root=0,task_indx=0;
	//All MPI-tasks loop over pivots, i.e. all but the last column which is b. 
	int current_col=0;
	for (int p = 0; p<(parms->num_col-2); p++){

		//Identify which MPI-tasks is going to be the root and Broadcast the pivot array.
		if (task_indx+1 < num_tasks){
			if (p >= parms->col_start_indx[task_indx+1]){
				if (task_indx < (num_tasks-1)){
					root++;
					task_indx++;
				}
			}
		}

		//Array holding pivots to be broadcasted to all MPI-tasks 
		float* pivots= (float*)check_malloc((parms->num_row-1)*sizeof(float),"Allocation of pivot array failed");
		for (int i=0; i<parms->num_row-1; i++)
			pivots[i]=0.0;

		//If root, prepare the pivot array to broadcast
		if (rank == root){
			for (int i =p; i< parms->num_row-1; i++){
				pivots[i] = parms->my_col[i+1][current_col]/parms->my_col[p][current_col];
			}
			current_col++;
		}
		check_flag(MPI_Bcast(pivots,(parms->num_row-1),MPI_FLOAT,root,MPI_COMM_WORLD),6);

		for (int i=1; i< parms->num_row; i++){
			for (int j=0; j< parms->num_my_col; j++){
				parms->my_col[i][j] = parms->my_col[i][j] - (pivots[i-1] * parms->my_col[p][j]);

			}
		}
	}
}

void gather_columns(int rank,int num_tasks,float** A){

	float *sendbuf,*recvbuf;
	sendbuf=(float*) check_malloc(parms->num_row * parms->num_my_col * sizeof(float),"Allocation failed for sendbuf in gather_columns");
	if (rank==MASTER){
		recvbuf=(float*) check_malloc(parms->num_row * parms->num_col * sizeof(float),"Allocation failed for recvbuf in gather_columns");
	}
	int sendcount = parms->num_row * parms->num_my_col , *disp, *recvcount;

	//All MPI-tasks fill the send buffer
	for( int j=0; j< parms->num_my_col; j++){
		for (int i=0; i< parms->num_row; i++){
			sendbuf[j*parms->num_row + i] = parms->my_col[i][j];
		}
	}

	if (rank==MASTER){
		recvcount=(int*) check_malloc(num_tasks * sizeof(int),"Allocation failed for recvcount in gather_columns");
		disp=(int*) check_malloc(num_tasks * sizeof(int),"Allocation failed for disp in gather_columns");
		int tmp_disp=0;
		for (int i=0; i < num_tasks; i++){
			disp[i]=tmp_disp;
			if (i < num_tasks -1){
				recvcount[i]=parms->num_row * (parms->col_start_indx[rank+1] - parms->col_start_indx[rank]);
				tmp_disp += parms->num_row * (parms->col_start_indx[rank+1] - parms->col_start_indx[rank]);
			}else if (i == num_tasks-1){
				recvcount[i]=parms->num_row * ( parms->num_col - parms->col_start_indx[rank]);
				tmp_disp += parms->num_row * ( parms->num_col - parms->col_start_indx[rank]);
			}	 	
		}
	}

	check_flag(MPI_Gatherv(sendbuf,sendcount,MPI_FLOAT,recvbuf,recvcount,disp,MPI_FLOAT,MASTER,MPI_COMM_WORLD),7);

	//Now populate the matrix A with the new values gathered from workers

	if (rank == MASTER){
		for (int j=0; j< parms->num_col; j++){
			for (int i=0; i< parms->num_row; i++){
				A[i][j] = recvbuf[j*parms->num_row +i];
			}	
		}
	free(recvbuf);
	free(disp);
	free(recvcount);
	printf("Gathered\n");
	}
}
