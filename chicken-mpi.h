
/* Chicken MPI interface. Based on the Caml/MPI interface by Xavier
 * Leroy, projet Cristal.
 */


#include <mpi.h>
#include <chicken.h>

typedef struct chicken_MPI_comm_struct {
     C_header tag;
     void *comm_data;
} chicken_MPI_comm_t;

static const C_header MPI_COMM_TAG = 
     ((sizeof(chicken_MPI_comm_t) - sizeof(C_header)) / sizeof(C_word)) | C_POINTER_TYPE;

#define Comm_val(x) (C_c_pointer_nn(x))


typedef struct chicken_MPI_group_struct {
     C_header tag;
     void *group_data;
} chicken_MPI_group_t;

static const C_header MPI_GROUP_TAG = 
     ((sizeof(chicken_MPI_group_t) - sizeof(C_header)) / sizeof(C_word)) | C_POINTER_TYPE;

#define Group_val(x) (C_c_pointer_nn(x))


#define C_8vector_length(x)         (C_header_size(C_block_item(x, 1)))
#define C_16vector_length(x)        (C_header_size(C_block_item(x, 1)) >> 1)
#define C_32vector_length(x)        (C_header_size(C_block_item(x, 1)) >> 2)
#define C_64vector_length(x)        (C_header_size(C_block_item(x, 1)) >> 3)
#define C_bytevector_length(x)      (C_header_size(x))

