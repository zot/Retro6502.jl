#include <stdlib.h>
#include "fake6502.h"

uint8_t (*readfunc)(fake6502_context *c, uint16_t addr);
void (*writefunc)(fake6502_context *c, uint16_t addr, uint8_t val);

fake6502_context* fake6502_init(void *read, void* write, void* state) {
  readfunc = read;
  writefunc = write;
  fake6502_context* ctx = calloc(1, sizeof(struct fake6502_context));
  ctx->state_host = state;
  fake6502_reset(ctx);
  return ctx;
}

uint8_t fake6502_mem_read(fake6502_context *c, uint16_t addr)
{
  return readfunc(c, addr);
}

void fake6502_mem_write(fake6502_context *c, uint16_t addr, uint8_t val)
{
  writefunc(c, addr, val);
}
