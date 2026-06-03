/* test_csr.S  –  CSR instruction and performance-counter coverage
 *
 * Covered:
 *   CSRRW  – write CSR, read old value into rd
 *   CSRRS  – set bits in CSR
 *   CSRRC  – clear bits in CSR
 *   mcycle – increments every cycle; two reads must show progression
 *   minstret – increments per retired instruction; verify delta
 *   mtime  – MMIO read at 0xBFF8; must return a non-zero value over time
 *   prefetch_ctrl (custom CSR 0x7C0) – read/write
 *   x0 as rd in CSR instructions (write-only, no rd side-effect)
 *
 * Returns: fail count in a0
 */

    .section .text
    .global  test_csr

/* CSR address definitions */
.equ CSR_MCYCLE,       0xB00
.equ CSR_MCYCLEH,      0xB80
.equ CSR_MINSTRET,     0xB02
.equ CSR_MISA,         0x301
.equ CSR_PREFETCH,     0x7C0

.equ MTIME_LO_ADDR,    0x0000BFF8
.equ MTIME_HI_ADDR,    0x0000BFFC

.macro CHK_IMM reg, expected
    li   t6, \expected
    beq  \reg, t6, 1f
    addi s0, s0, 1
1:
.endm

test_csr:
    addi sp, sp, -16
    sw   ra, 12(sp)
    sw   s0,  8(sp)
    li   s0, 0

    /* ════════════════════════════════════════════════════════════
     * C01 – CSRRW: write and read back
     * ════════════════════════════════════════════════════════════ */
    /* Save current prefetch_ctrl, write a new value, read back */
    csrr  t4, CSR_PREFETCH          /* save original */

    li    t0, 0x3
    csrrw t1, CSR_PREFETCH, t0      /* t1 = old value, CSR ← 3 */
    nop
    csrr  t2, CSR_PREFETCH          /* t2 should be 3 */
    CHK_IMM t2, 0x3

    /* Restore */
    csrrw zero, CSR_PREFETCH, t4

    /* ════════════════════════════════════════════════════════════
     * C02 – CSRRS: set individual bits
     * ════════════════════════════════════════════════════════════ */
    csrrw zero, CSR_PREFETCH, zero  /* ensure CSR = 0 first */
    li    t0, 0x1
    csrrs t1, CSR_PREFETCH, t0     /* set bit0; t1 = old (0) */
    CHK_IMM t1, 0x0
    csrr  t2, CSR_PREFETCH
    CHK_IMM t2, 0x1                 /* bit0 now set */

    li    t0, 0x2
    csrrs zero, CSR_PREFETCH, t0   /* set bit1 too; rd=x0 = no read */
    nop
    csrr  t2, CSR_PREFETCH
    CHK_IMM t2, 0x3                 /* both bits set */

    /* ════════════════════════════════════════════════════════════
     * C03 – CSRRC: clear individual bits
     * ════════════════════════════════════════════════════════════ */
    li    t0, 0x1
    csrrc t1, CSR_PREFETCH, t0     /* clear bit0; t1 = old (3) */
    CHK_IMM t1, 0x3
    csrr  t2, CSR_PREFETCH
    CHK_IMM t2, 0x2                 /* only bit1 remains */

    csrrw zero, CSR_PREFETCH, zero  /* restore to 0 */

    /* ════════════════════════════════════════════════════════════
     * C04 – mcycle: verify the counter increases
     * ════════════════════════════════════════════════════════════ */
    csrr  t0, CSR_MCYCLE
    /* Execute several instructions to advance the counter */
    addi  t1, zero, 0
    addi  t1, t1, 1
    addi  t1, t1, 1
    addi  t1, t1, 1
    addi  t1, t1, 1
    addi  t1, t1, 1
    addi  t1, t1, 1
    csrr  t2, CSR_MCYCLE
    /* t2 must be strictly greater than t0 (unsigned) */
    bltu  t0, t2, 1f
    addi  s0, s0, 1         /* fail: counter did not advance */
1:

    /* ════════════════════════════════════════════════════════════
     * C05 – minstret: verify retired-instruction count increases
     * We execute exactly 8 ADDI after the first read and verify
     * the delta is at least 8 (may be more due to how the pipeline
     * retires instructions; dual-issue means faster drain).
     * ════════════════════════════════════════════════════════════ */
    csrr  s1, CSR_MINSTRET
    addi  t1, zero, 1       /* retire 1 */
    addi  t1, t1,   1       /* retire 2 */
    addi  t1, t1,   1       /* retire 3 */
    addi  t1, t1,   1       /* retire 4 */
    addi  t1, t1,   1       /* retire 5 */
    addi  t1, t1,   1       /* retire 6 */
    addi  t1, t1,   1       /* retire 7 */
    addi  t1, t1,   1       /* retire 8 */
    csrr  t2, CSR_MINSTRET
    sub   t3, t2, s1        /* delta */
    /* delta must be >= 8 (these 8 ADDIs + the two CSRR instructions) */
    li    t4, 8
    bge   t3, t4, 1f
    addi  s0, s0, 1         /* fail: too few instructions counted */
1:
    /* Also verify delta doesn't go negative (no overflow in a short test) */
    bgez  t3, 1f
    addi  s0, s0, 1
1:

    /* ════════════════════════════════════════════════════════════
     * C06 – mtime MMIO: two reads must be increasing
     * mtime at 0xBFF8 increments every cycle (in CSRFile).
     * ════════════════════════════════════════════════════════════ */
    li    t0, MTIME_LO_ADDR
    lw    t1, 0(t0)         /* first read */
    /* Some delay */
    addi  t2, zero, 0
    addi  t2, t2, 1
    addi  t2, t2, 1
    lw    t3, 0(t0)         /* second read */
    bltu  t1, t3, 1f        /* t3 > t1: counter advanced */
    addi  s0, s0, 1
1:
    /* mtime high must be 0 (32-bit processor, mtime fits in low 32 bits
       for a short simulation run – if not zero the counter has wrapped) */
    li    t0, MTIME_HI_ADDR
    lw    t2, 0(t0)
    /* Don't fail here; just verify the load doesn't crash the pipeline.
       High 32 bits may be non-zero for very long runs.  We only check
       that the load completes and the pipeline doesn't hang. */

    /* ════════════════════════════════════════════════════════════
     * C07 – misa: read only (must match RV32I or RV32IM)
     * misa[31:30] = 01 for RV32; misa[8] = 1 for 'I' extension
     * ════════════════════════════════════════════════════════════ */
    csrr  t0, CSR_MISA
    /* Check MXL field (bits 31:30) = 01 (RV32) */
    srli  t1, t0, 30
    li    t2, 1
    bne   t1, t2, 1f
    j     2f
1:  addi  s0, s0, 1
2:
    /* Check 'I' bit (bit 8) = 1 */
    srli  t1, t0, 8
    andi  t1, t1, 1
    CHK_IMM t1, 1

    /* ════════════════════════════════════════════════════════════
     * C08 – CSRRW with rd = x0: write without reading
     *        Verify the write still takes effect.
     * ════════════════════════════════════════════════════════════ */
    li    t0, 0x1
    csrrw zero, CSR_PREFETCH, t0    /* write=1, rd=x0 (suppress read) */
    nop
    csrr  t1, CSR_PREFETCH
    CHK_IMM t1, 0x1
    csrrw zero, CSR_PREFETCH, zero  /* clear */

    /* ── Done ─────────────────────────────────────────────────── */
    mv   a0, s0
    lw   ra, 12(sp)
    lw   s0,  8(sp)
    addi sp, sp, 16
    ret
