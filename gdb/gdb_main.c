#include <assert.h>
#include <unistd.h>
#include <fcntl.h>

#include "sail.h"
#include "gdb_rsp.h"
#include "gdb_utils.h"

/* Sail generated functions */

void model_init(void);
unit zinitializze_model(unit);
unit zinitialise_64_bit_mode(unit);
unit zx86_fetch_decode_execute(unit);
uint64_t zread_rip(unit);
unit zwrite_rip(uint64_t);

enum kind_zexception { Kind_zEmsg, Kind_zSyscall };

struct zexception {
  enum kind_zexception kind;
  union {
    struct { sail_string zEmsg; };
    struct { unit zSyscall; };
  };
};

extern struct zexception *current_exception;
extern bool have_exception;
extern sail_string *throw_location;

enum gdb_reg_type { gdb_sbits, gdb_fbits, gdb_lbits };
struct gdb_register_info { char *name; enum gdb_reg_type ty; int size; void *reg; void (*read_fn)(void *dst, void *reg); void (*write_fn)(
void *src, void *reg); };

extern int gdb_register_count;
extern struct gdb_register_info gdb_registers[];


int step(struct rsp_conn *conn, struct sail_arch *arch) {
  zx86_fetch_decode_execute(UNIT);
  if (have_exception) {
    switch (current_exception->kind) {
    case Kind_zEmsg:
      dprintf(conn->log_fd, "Model exception: %s at %s\n", current_exception->zEmsg, *throw_location);
      break;
    case Kind_zSyscall:
      dprintf(conn->log_fd, "syscall encountered at %s\n", *throw_location);
      break;
    default:
      dprintf(conn->log_fd, "unknown internal Sail exception at %s, exiting!\n", *throw_location);
      break;
    }
    conn_exit(conn, 1);
  }
  return 0;
}

mach_bits get_pc(struct rsp_conn *conn, struct sail_arch *arch) {
  return zread_rip(UNIT);
}

void set_pc(struct rsp_conn *conn, struct sail_arch *arch, mach_bits regval) {
  zwrite_rip(regval);
}

int get_reg(struct rsp_conn *conn, struct sail_arch *arch, mpz_t result, uint64_t regno) {
  assert (regno < gdb_register_count);
  if (gdb_registers[regno].read_fn) {
    assert(gdb_registers[regno].ty == gdb_lbits); // TODO: others
    lbits read;
    read.bits = (mpz_t *) result;
    read.len = 0;
    gdb_registers[regno].read_fn((void *)&read, gdb_registers[regno].reg);
    return read.len;
  } else if (gdb_registers[regno].ty == gdb_fbits) {
    mpz_set_ui(result, *(int64_t *)gdb_registers[regno].reg);
    return gdb_registers[regno].size;
  } else if (gdb_registers[regno].ty == gdb_lbits) {
    mpz_set(result, *((lbits *)gdb_registers[regno].reg)->bits);
    return gdb_registers[regno].size;
  } else {
    assert(false);
  }
}

// TODO: check number of bits
void set_reg(struct rsp_conn *conn, struct sail_arch *arch, uint64_t regno, const mpz_t regval) {
  assert (regno < gdb_register_count);
  if (gdb_registers[regno].write_fn) {
    assert(gdb_registers[regno].ty == gdb_lbits); // TODO: others
    lbits v;
    v.bits = (mpz_t *) regval;
    v.len = gdb_registers[regno].size;
    gdb_registers[regno].write_fn((void *)&v, gdb_registers[regno].reg);
  } else if (gdb_registers[regno].ty == gdb_fbits) {
    *(int64_t *)gdb_registers[regno].reg = mpz_get_ui(regval);
  } else if (gdb_registers[regno].ty == gdb_lbits) {
    mpz_set(*((lbits *)gdb_registers[regno].reg)->bits, regval);
  } else {
    assert(false);
  }
}

bool is_done(struct rsp_conn *conn, struct sail_arch *arch) {
  return false;
}

extern bool zapp_view;
extern bool zms_reg;
extern bool zfault_reg;
 
int main(int argc, char *argv[]) {
  struct sail_arch arch = {
    .archlen = ARCH64,
    .nregs = gdb_register_count,
    .get_reg = get_reg,
    .set_reg = set_reg,
    .get_pc = get_pc,
    .set_pc = set_pc,
    .step = step,
    .is_done = is_done,
    .arch_info = 0,
  };

  int opt;
  int verbose = 1;
  while ((opt = getopt(argc, argv, "q")) != -1) {
    switch (opt) {
    case 'q':
      verbose = 0;
      break;
    default:
      fprintf(stderr, "Usage: %s [-q]\n", argv[0]);
      exit(1);
    }
  }

  model_init();
  zapp_view = true;
  zms_reg = false;
  zfault_reg = false;
  zinitializze_model(UNIT);
  zinitialise_64_bit_mode(UNIT);
  int log_fd = 2;
  if (!verbose) {
    log_fd = open("/dev/null", O_WRONLY);
  }
  struct rsp_conn *conn = gdb_server_init(1234, log_fd);
  gdb_server_set_arch(conn, &arch);
  gdb_server_run(conn);
  // Doesn't return (currently)
  return 0;
}
