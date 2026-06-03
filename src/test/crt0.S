/* crt0.S  –  Reset handler, BSS clear, test runner, I/O helpers
 *
 * Register convention inside each test_xxx function:
 *   s0  = local fail counter (callee-saved, must be saved/restored)
 *   a0  = return value (fail count for that suite)
 *   t0-t6, a1-a7 = scratch (caller-saved)
 *
 * The top-level runner accumulates fail counts in s8.
 */

    .section .text.init
    .global  _start

_start:
    /* ── 1. set up stack ─────────────────────────────────────── */
    la   sp, _stack_top

    /* ── 2. zero BSS ─────────────────────────────────────────── */
    la   t0, _bss_start
    la   t1, _bss_end
.Lbss_loop:
    bge  t0, t1, .Lbss_done
    sw   zero, 0(t0)
    addi t0, t0, 4
    j    .Lbss_loop
.Lbss_done:
    addi x8, x2, 0

    /* ── 3. run test suites ───────────────────────────────────── */
    li   s8, 0          /* global fail counter */

    call test_alu
    add  s8, s8, a0

    call test_mem
    add  s8, s8, a0

    call test_branch
    add  s8, s8, a0

    call test_hazard
    add  s8, s8, a0

    call test_csr
    add  s8, s8, a0

    call test_cache
    add  s8, s8, a0

    bnez s8, .Lsuite_fail

    /* ── 4. report result ─────────────────────────────────────── */
    /* All passed: print "PASS\n" */
    li   a0, 'P'; call _putchar
    li   a0, 'A'; call _putchar
    li   a0, 'S'; call _putchar
    li   a0, 'S'; call _putchar
    li   a0, '\n'; call _putchar
    j    .Ldone

.Lsuite_fail:
    li   a0, 'F'
    call _putchar
    li   a0, '\n'; call _putchar

.Ldone:
    /* ── 5. assert success MMIO (io.success ← 1) ─────────────── */
    li   t0, 0x10001FF0
    li   t1, 1
    sw   t1, 0(t0)

.Lhalt:
    j    .Lhalt


/* ═══════════════════════════════════════════════════════════════
 * _putchar(a0: char)  –  write one byte to printf MMIO
 * Clobbers: t0
 * ═══════════════════════════════════════════════════════════════ */
    .global _putchar
_putchar:
    li   t0, 0x10001FF1
    sb   a0, 0(t0)
    ret


/* ═══════════════════════════════════════════════════════════════
 * _printdec(a0: uint)  –  print decimal, 0..9999
 * Saves: ra, s0, s1
 * ═══════════════════════════════════════════════════════════════ */
    .global _printdec
_printdec:
    addi sp, sp, -16
    sw   ra, 12(sp)
    sw   s0,  8(sp)
    sw   s1,  4(sp)
    mv   s0, a0
    li   s1, 0          /* leading-zero flag */

    /* thousands */
    li   t1, 1000
    li   t2, 0
.Lthousands:
    blt  s0, t1, .Lt_done
    addi t2, t2, 1
    sub  s0, s0, t1
    j    .Lthousands
.Lt_done:
    bnez t2, 1f
    bnez s1, 1f
    j    2f
1:  addi a0, t2, '0'; call _putchar; li s1, 1
2:
    /* hundreds */
    li   t1, 100
    li   t2, 0
.Lhundreds:
    blt  s0, t1, .Lh_done
    addi t2, t2, 1
    sub  s0, s0, t1
    j    .Lhundreds
.Lh_done:
    bnez t2, 1f
    bnez s1, 1f
    j    2f
1:  addi a0, t2, '0'; call _putchar; li s1, 1
2:
    /* tens */
    li   t1, 10
    li   t2, 0
.Ltens:
    blt  s0, t1, .Lten_done
    addi t2, t2, 1
    sub  s0, s0, t1
    j    .Ltens
.Lten_done:
    bnez t2, 1f
    bnez s1, 1f
    j    2f
1:  addi a0, t2, '0'; call _putchar
2:
    /* ones – always print */
    addi a0, s0, '0'
    call _putchar

    lw   ra, 12(sp)
    lw   s0,  8(sp)
    lw   s1,  4(sp)
    addi sp, sp, 16
    ret
