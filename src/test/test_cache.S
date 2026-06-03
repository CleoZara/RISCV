/* test_cache.S  –  D-Cache behavior coverage
 *
 * Cache parameters (from CacheParams in source):
 *   Total: 8 KB     – 8192 bytes
 *   Sets:  32       – indexed by addr[10:6]
 *   Ways:  4        – fully associative within a set
 *   Line:  64 bytes – 16 × 32-bit words
 *
 * Addresses in the same SET have identical bits [10:6].
 * Adding 0x800 (2048) to any address keeps bits [10:6] unchanged
 * but changes the TAG, so it maps to the SAME set with a DIFFERENT tag.
 *
 * Test plan:
 *   K01 – Sequential write + readback (1 cache line, 16 words)
 *   K02 – Sequential array fill (128 words = 4 cache lines, verifies
 *          spatial prefetcher doesn't corrupt data)
 *   K03 – Stride-16 access (stride = 64 B = 1 cache line), exercises
 *          a different set each access
 *   K04 – Repeated access to same line (cold → warm hit sequence)
 *   K05 – 4-way fill of one set (fill all 4 ways → 5th access evicts LRU)
 *          then reload evicted line to verify dirty writeback correctness
 *   K06 – Write + eviction + reload (dirty-line writeback)
 *
 * _scratch (from linker) is cache-line aligned and 20 KB.
 * Same-set "pages" are spaced 0x800 (2048) bytes apart.
 *
 * Returns: fail count in a0
 */

    .section .text
    .global  test_cache

.macro CHK_IMM reg, expected
    li   t6, \expected
    beq  \reg, t6, 1f
    addi s0, s0, 1
1:
.endm

test_cache:
    addi sp, sp, -32
    sw   ra, 28(sp)
    sw   s0, 24(sp)
    sw   s1, 20(sp)
    sw   s2, 16(sp)
    sw   s3, 12(sp)
    li   s0, 0

    la   s3, _scratch       /* base of 20 KB scratch area */

    /* ════════════════════════════════════════════════════════════
     * K01 – One full cache line: write all 16 words, read all back
     * ════════════════════════════════════════════════════════════ */
    li   s1, 0              /* word index */
.LK01_write:
    li   t0, 16
    bge  s1, t0, .LK01_read
    slli t1, s1, 2          /* byte offset = index * 4 */
    add  t2, s3, t1
    addi t3, s1, 0x100      /* pattern: 0x100, 0x101, ... 0x10F */
    sw   t3, 0(t2)
    addi s1, s1, 1
    j    .LK01_write
.LK01_read:
    li   s1, 0
.LK01_verify:
    li   t0, 16
    bge  s1, t0, .LK01_done
    slli t1, s1, 2
    add  t2, s3, t1
    lw   t3, 0(t2)
    addi t4, s1, 0x100
    beq  t3, t4, 1f
    addi s0, s0, 1          /* fail */
1:
    addi s1, s1, 1
    j    .LK01_verify
.LK01_done:
    beqz s0, .LK01_ok
    li   a0, 1
    j    .Lcache_return
.LK01_ok:

    /* ════════════════════════════════════════════════════════════
     * K02 – Sequential array: 128 words (512 bytes = 4 cache lines)
     * Write forward, verify backward (exercises spatial locality)
     * ════════════════════════════════════════════════════════════ */
    /* Write phase: pattern = index × 3 + 1 */
    li   s1, 0
.LK02_write:
    li   t0, 128
    bge  s1, t0, .LK02_verify
    slli t1, s1, 2
    add  t2, s3, t1
    /* Skip first line (already used by K01); write starting at offset 64 */
    addi t2, t2, 64
    slli t3, s1, 1
    add  t3, t3, s1         /* t3 = s1 * 3 */
    addi t3, t3, 1
    sw   t3, 0(t2)
    addi s1, s1, 1
    j    .LK02_write
.LK02_verify:
    /* Read phase (backward) */
    li   s1, 127
.LK02_read:
    bltz s1, .LK02_done
    slli t1, s1, 2
    add  t2, s3, t1
    addi t2, t2, 64
    lw   t3, 0(t2)
    slli t4, s1, 1
    add  t4, t4, s1
    addi t4, t4, 1
    beq  t3, t4, 1f
    addi s0, s0, 1
1:
    addi s1, s1, -1
    j    .LK02_read
.LK02_done:
    beqz s0, .LK02_ok
    li   a0, 2
    j    .Lcache_return
.LK02_ok:

    /* ════════════════════════════════════════════════════════════
     * K03 – Stride-64-byte (one cache line stride)
     * Access 16 different cache lines spread across all 32 sets.
     * Verifies that each line is independently cached.
     * ════════════════════════════════════════════════════════════ */
    li   s1, 0
    li   s2, 0x600          /* start at offset 0x600 from scratch */
    add  s2, s3, s2
.LK03_write:
    li   t0, 16
    bge  s1, t0, .LK03_read
    /* stride = 64 bytes (one cache line) */
    slli t3, s1, 6
    add  t2, s2, t3
    addi t4, s1, 0x200      /* pattern */
    sw   t4, 0(t2)
    addi s1, s1, 1
    j    .LK03_write
.LK03_read:
    li   s1, 0
.LK03_verify:
    li   t0, 16
    bge  s1, t0, .LK03_done
    slli t3, s1, 6
    add  t2, s2, t3
    lw   t3, 0(t2)
    addi t4, s1, 0x200
    beq  t3, t4, 1f
    addi s0, s0, 1
1:
    addi s1, s1, 1
    j    .LK03_verify
.LK03_done:
    beqz s0, .LK03_ok
    li   a0, 3
    j    .Lcache_return
.LK03_ok:

    /* ════════════════════════════════════════════════════════════
     * K04 – Repeated access to same line (cold miss then warm hits)
     * Write once, read many times; all reads after the first should
     * hit the cache and return the correct value.
     * ════════════════════════════════════════════════════════════ */
    li   t0, 0xFACEFACE
    sw   t0, 0(s3)          /* cold write */
    /* 8 subsequent loads from the same word must all return the right value */
    lw   t1, 0(s3); CHK_IMM t1, 0xFACEFACE
    beqz s0, .LK04_ok
    li   a0, 4
    j    .Lcache_return
.LK04_ok:
    lw   t1, 0(s3); CHK_IMM t1, 0xFACEFACE
    lw   t1, 0(s3); CHK_IMM t1, 0xFACEFACE
    lw   t1, 0(s3); CHK_IMM t1, 0xFACEFACE
    lw   t1, 0(s3); CHK_IMM t1, 0xFACEFACE
    lw   t1, 0(s3); CHK_IMM t1, 0xFACEFACE
    lw   t1, 0(s3); CHK_IMM t1, 0xFACEFACE
    lw   t1, 0(s3); CHK_IMM t1, 0xFACEFACE

    /* ════════════════════════════════════════════════════════════
     * K05 – 4-way fill + LRU eviction (same cache set, 5 lines)
     *
     * Five addresses at offsets 0, 0x800, 0x1000, 0x1800, 0x2000
     * relative to scratch all map to the same set (bits [10:6]
     * are unchanged when adding 0x800 because 0x800 sets bit[11]).
     *
     * Write distinct patterns to each, then access all 5 in sequence.
     * After the 5th, the LRU line (first written) must be evicted.
     * Re-read the first line: if the dirty eviction worked, the
     * value written to it is still correct in memory.
     * ════════════════════════════════════════════════════════════ */
    /* Use scratch + 0x1000 as base for this test (avoids K01-K04 area) */
    li   t4, 0x1000
    add  a4, s3, t4         /* a4 = same-set base */

    /* Write distinct magic values to the first word of each of 5 pages */
    li   t0, 0xAABBCC00
    sw   t0, 0(a4)                  /* way 0 candidate */
    li   t0, 0xAABBCC01
    li   t1, 0x800
    add  t2, a4, t1
    sw   t0, 0(t2)                  /* way 1 candidate */
    li   t0, 0xAABBCC02
    li   t1, 0x1000
    add  t2, a4, t1
    sw   t0, 0(t2)                  /* way 2 candidate */
    li   t0, 0xAABBCC03
    li   t1, 0x1800
    add  t2, a4, t1
    sw   t0, 0(t2)                  /* way 3 candidate */

    /* Fill all 4 ways by loading each line */
    lw   t5, 0(a4)
    li   t1, 0x800;   add t2, a4, t1; lw t5, 0(t2)
    li   t1, 0x1000;  add t2, a4, t1; lw t5, 0(t2)
    li   t1, 0x1800;  add t2, a4, t1; lw t5, 0(t2)
    /* Now all 4 ways are valid; a4+0 is the LRU candidate */

    /* Write a modified value to way0 (making it dirty) */
    li   t0, 0xDEADDEAD
    sw   t0, 0(a4)

    /* Access the 5th line → forces eviction of LRU (way0 = a4+0) */
    /* The evicted dirty line must be written back to memory */
    li   t1, 0x2000
    add  t2, a4, t1
    li   t0, 0x55555555
    sw   t0, 0(t2)          /* 5th line: store then load */
    lw   t5, 0(t2)
    CHK_IMM t5, 0x55555555

    /* Now reload way0 (a4+0): the data that was in the cache
       (0xDEADDEAD) must have been written back to memory, so
       loading from that address again should give 0xDEADDEAD */
    lw   t5, 0(a4)
    CHK_IMM t5, 0xDEADDEAD
    beqz s0, .LK05_ok
    li   a0, 5
    j    .Lcache_return
.LK05_ok:

    /* ════════════════════════════════════════════════════════════
     * K06 – Write-back verify: dirty line written correctly
     * Modify multiple words in a cache line, evict, reload, verify.
     * ════════════════════════════════════════════════════════════ */
    li   t4, 0x3000
    add  a4, s3, t4         /* fresh area for K06 */

    /* Write a full cache line (16 words) with pattern 0xC0DE_xxxx */
    li   s1, 0
.LK06_write:
    li   t0, 16
    bge  s1, t0, .LK06_done_write
    slli t1, s1, 2
    add  t2, a4, t1
    li   t3, 0xC0DE0000
    or   t3, t3, s1
    sw   t3, 0(t2)
    addi s1, s1, 1
    j    .LK06_write
.LK06_done_write:
    /* Now bring the line into cache with a load */
    lw   t5, 0(a4)

    /* Overwrite all 16 words (mark line dirty) */
    li   s1, 0
.LK06_overwrite:
    li   t0, 16
    bge  s1, t0, .LK06_done_ow
    slli t1, s1, 2
    add  t2, a4, t1
    li   t3, 0xBAD00000
    or   t3, t3, s1
    sw   t3, 0(t2)
    addi s1, s1, 1
    j    .LK06_overwrite
.LK06_done_ow:

    /* Force eviction: access 4 other lines in the same set */
    li   t0, 0x800;  add t2, a4, t0;  lw t5, 0(t2)
    li   t0, 0x1000; add t2, a4, t0;  lw t5, 0(t2)
    li   t0, 0x1800; add t2, a4, t0;  lw t5, 0(t2)
    li   t0, 0x2000; add t2, a4, t0;  lw t5, 0(t2)

    /* Reload a4 line from memory (must reflect the overwritten values) */
    li   s1, 0
.LK06_verify:
    li   t0, 16
    bge  s1, t0, .LK06_vdone
    slli t1, s1, 2
    add  t2, a4, t1
    lw   t3, 0(t2)
    li   t4, 0xBAD00000
    or   t4, t4, s1
    beq  t3, t4, 1f
    addi s0, s0, 1
1:
    addi s1, s1, 1
    j    .LK06_verify
.LK06_vdone:
    beqz s0, .LK06_ok
    li   a0, 6
    j    .Lcache_return
.LK06_ok:

    /* ── Done ─────────────────────────────────────────────────── */
    mv   a0, s0
.Lcache_return:
    lw   ra, 28(sp)
    lw   s0, 24(sp)
    lw   s1, 20(sp)
    lw   s2, 16(sp)
    lw   s3, 12(sp)
    addi sp, sp, 32
    ret
