/* test_mem.S  –  Load/Store instruction coverage
 *
 * Covered:
 *   SW / LW     – full word, 4-byte aligned
 *   SH / LH     – half-word (signed), offsets 0 and 2
 *   SH / LHU    – half-word (unsigned), sign-extension check
 *   SB / LB     – byte (signed), all 4 byte offsets
 *   SB / LBU    – byte (unsigned), sign-extension check
 *   Negative value store and sign-extension on load
 *   Store-then-load same address (through cache)
 *   Back-to-back stores to adjacent words
 *   Unaligned byte/halfword within same cache line
 *
 * Returns: fail count in a0
 */

    .section .bss
    .align 6                /* 64-byte (cache line) alignment */
_mem_buf:
    .space 128              /* 2 cache lines of scratch space */

    .section .text
    .global  test_mem

.macro CHK_IMM expected
    li   t3, \expected
    beq  t2, t3, 1f
    addi s0, s0, 1
1:
.endm

test_mem:
    addi sp, sp, -16
    sw   ra, 12(sp)
    sw   s0,  8(sp)
    li   s0, 0

    la   a5, _mem_buf       /* base address for all memory tests */

    /* ────────────────────────────────────────────────────────────
     * SW / LW
     * ──────────────────────────────────────────────────────────── */
    li   t0, 0x12345678
    sw   t0, 0(a5)
    lw   t2, 0(a5);         CHK_IMM 0x12345678

    li   t0, 0xDEADBEEF
    sw   t0, 4(a5)
    lw   t2, 4(a5);         CHK_IMM 0xDEADBEEF   /* MSB set – full 32-bit */

    li   t0, 0
    sw   t0, 8(a5)
    lw   t2, 8(a5);         CHK_IMM 0

    li   t0, -1
    sw   t0, 12(a5)
    lw   t2, 12(a5);        CHK_IMM 0xFFFFFFFF   /* -1 round-trips */

    /* Back-to-back adjacent word stores */
    li   t0, 0xAAAAAAAA
    li   t1, 0x55555555
    sw   t0, 16(a5)
    sw   t1, 20(a5)
    lw   t2, 16(a5);        CHK_IMM 0xAAAAAAAA
    lw   t2, 20(a5);        CHK_IMM 0x55555555

    /* ────────────────────────────────────────────────────────────
     * SH / LH (signed half-word load)
     * ──────────────────────────────────────────────────────────── */
    /* Write positive half at offset 0 */
    li   t0, 0x0ABC
    sh   t0, 0(a5)
    lh   t2, 0(a5);         CHK_IMM 0x0ABC       /* positive, no sign ext */

    /* Write negative half (MSB set) at offset 0 */
    li   t0, 0x8001
    sh   t0, 0(a5)
    lh   t2, 0(a5);         CHK_IMM 0xFFFF8001   /* sign-extended */

    /* Half at byte-offset 2 within word */
    li   t0, 0x1234
    sh   t0, 2(a5)
    lh   t2, 2(a5);         CHK_IMM 0x1234

    li   t0, 0xFFFF
    sh   t0, 2(a5)
    lh   t2, 2(a5);         CHK_IMM -1           /* 0xFFFF sign-ext to -1 */

    /* ────────────────────────────────────────────────────────────
     * SH / LHU (unsigned half-word load)
     * ──────────────────────────────────────────────────────────── */
    li   t0, 0x8001
    sh   t0, 0(a5)
    lhu  t2, 0(a5);         CHK_IMM 0x8001       /* no sign extension */

    li   t0, 0xFFFF
    sh   t0, 2(a5)
    lhu  t2, 2(a5);         CHK_IMM 0xFFFF       /* 65535, not -1 */

    /* ────────────────────────────────────────────────────────────
     * SB / LB (signed byte load), all 4 offsets within a word
     * ──────────────────────────────────────────────────────────── */
    /* First clear the word */
    sw   zero, 0(a5)

    li   t0, 0x41            /* 'A' */
    sb   t0, 0(a5)
    lb   t2, 0(a5);         CHK_IMM 0x41        /* positive, no sign ext */

    li   t0, 0x80            /* negative byte */
    sb   t0, 0(a5)
    lb   t2, 0(a5);         CHK_IMM 0xFFFFFF80  /* sign-extended to -128 */

    li   t0, 0x7F
    sb   t0, 1(a5)
    lb   t2, 1(a5);         CHK_IMM 0x7F

    li   t0, 0xFF
    sb   t0, 2(a5)
    lb   t2, 2(a5);         CHK_IMM -1          /* 0xFF sign-ext = -1 */

    li   t0, 0x55
    sb   t0, 3(a5)
    lb   t2, 3(a5);         CHK_IMM 0x55

    /* Verify the four bytes are independently stored */
    li   t0, 0xAA
    sb   t0, 0(a5)
    li   t0, 0xBB
    sb   t0, 1(a5)
    li   t0, 0xCC
    sb   t0, 2(a5)
    li   t0, 0xDD
    sb   t0, 3(a5)
    lw   t2, 0(a5);         CHK_IMM 0xDDCCBBAA  /* little-endian */

    /* ────────────────────────────────────────────────────────────
     * SB / LBU (unsigned byte load)
     * ──────────────────────────────────────────────────────────── */
    li   t0, 0x80
    sb   t0, 0(a5)
    lbu  t2, 0(a5);         CHK_IMM 0x80        /* no sign extension */

    li   t0, 0xFF
    sb   t0, 3(a5)
    lbu  t2, 3(a5);         CHK_IMM 0xFF        /* 255, not -1 */

    /* ────────────────────────────────────────────────────────────
     * Store-then-load through cache: verify cache coherence
     * ──────────────────────────────────────────────────────────── */
    li   t0, 0x11223344
    sw   t0, 32(a5)
    /* Some intervening ALU work to ensure the load sees the stored value */
    li   t1, 0
    addi t1, t1, 1
    addi t1, t1, 1
    lw   t2, 32(a5);        CHK_IMM 0x11223344

    /* Write two words, overwrite the first, read both */
    li   t0, 0xAAAA
    sw   t0, 36(a5)
    li   t0, 0xBBBB
    sw   t0, 40(a5)
    li   t0, 0xCCCC
    sw   t0, 36(a5)         /* overwrite first */
    lw   t2, 36(a5);        CHK_IMM 0xCCCC
    lw   t2, 40(a5);        CHK_IMM 0xBBBB

    /* ────────────────────────────────────────────────────────────
     * Mixed-width to same word: SH then read with LBU
     * ──────────────────────────────────────────────────────────── */
    sw   zero, 44(a5)
    li   t0, 0x1234
    sh   t0, 44(a5)         /* write half at offset 0 in that word */
    lbu  t2, 44(a5);        CHK_IMM 0x34        /* little-endian: low byte first */
    lbu  t2, 45(a5);        CHK_IMM 0x12        /* high byte of half */

    /* ────────────────────────────────────────────────────────────
     * Large immediate offset (uses S-type encoding)
     * ──────────────────────────────────────────────────────────── */
    li   t0, 0x99887766
    sw   t0, 60(a5)
    lw   t2, 60(a5);        CHK_IMM 0x99887766

    /* ── Done ─────────────────────────────────────────────────── */
    mv   a0, s0
    lw   ra, 12(sp)
    lw   s0,  8(sp)
    addi sp, sp, 16
    ret
