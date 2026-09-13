
/* Chicken MPI interface. Based on the Caml/MPI interface by Xavier
 * Leroy, projet Cristal.
 */


#include <stdint.h>
#include <mpi.h>
#include <chicken.h>

#ifndef C_c_u8vector
#define C_c_u8vector(x) ((unsigned char *)C_data_pointer(x))
#endif

/*
  MPI_Comm/MPI_Group/MPI_Datatype/MPI_Win are opaque handles, but
  their underlying representation differs by implementation: a pointer
  under Open MPI, a plain int under MPICH. This module always carries
  them boxed as a C_word-sized value, so extracting one has to
  round-trip through intptr_t, which is a type guaranteed wide enough
  to hold a pointer, rather than casting directly to/from the handle
  type. Casting an int handle straight to/from void* could silently
  truncate or widen across a pointer/int size mismatch, since the two
  need not be the same width.
*/

typedef struct chicken_MPI_comm_struct {
     C_header tag;
     void *comm_data;
} chicken_MPI_comm_t;

static const C_header MPI_COMM_TAG =
     ((sizeof(chicken_MPI_comm_t) - sizeof(C_header)) / sizeof(C_word)) | C_POINTER_TYPE;

#define Comm_val(x) ((MPI_Comm)(intptr_t)C_c_pointer_nn(x))


typedef struct chicken_MPI_group_struct {
     C_header tag;
     void *group_data;
} chicken_MPI_group_t;

static const C_header MPI_GROUP_TAG =
     ((sizeof(chicken_MPI_group_t) - sizeof(C_header)) / sizeof(C_word)) | C_POINTER_TYPE;

#define Group_val(x) ((MPI_Group)(intptr_t)C_c_pointer_nn(x))


typedef struct chicken_MPI_datatype_struct {
     C_header tag;
     MPI_Datatype *datatype_data;
} chicken_MPI_datatype_t;

static const C_header MPI_DATATYPE_TAG =
     ((sizeof(chicken_MPI_datatype_t) - sizeof(C_header)) / sizeof(C_word)) | C_POINTER_TYPE;

#define Datatype_val(x) ((MPI_Datatype)(intptr_t)C_c_pointer_nn(x))


typedef struct chicken_MPI_window_struct {
     C_header tag;
     MPI_Win *window_data;
} chicken_MPI_window_t;

static const C_header MPI_WINDOW_TAG =
     ((sizeof(chicken_MPI_window_t) - sizeof(C_header)) / sizeof(C_word)) | C_POINTER_TYPE;

#define Window_val(x) ((MPI_Win)(intptr_t)C_c_pointer_nn(x))


#define C_8vector_length(x)         (C_header_size(C_block_item(x, 1)))
#define C_16vector_length(x)        (C_header_size(C_block_item(x, 1)) >> 1)
#define C_32vector_length(x)        (C_header_size(C_block_item(x, 1)) >> 2)
#define C_64vector_length(x)        (C_header_size(C_block_item(x, 1)) >> 3)
#define C_bytevector_length(x)      (C_header_size(x))

