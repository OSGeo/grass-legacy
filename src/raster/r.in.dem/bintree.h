#define CACHE_SIZE 100 /* Number of entries in the write queue */
#define PTREE_SIZE 64000

typedef short cell_data_t;
typedef unsigned short cell_row_t;

struct ptree{
  cell_row_t row;
  cell_row_t col;
  cell_data_t data;
};

int cache(const cell_row_t row, const cell_row_t col, const cell_data_t data);
void insert_ptree_node(const cell_row_t row, const cell_row_t col, const cell_data_t data);
short dump_ptree();
short bubble_up(const cell_row_t row, const cell_row_t col, const cell_data_t data, unsigned short offset);
void set_queue_fd (const int fd);










