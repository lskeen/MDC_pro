package matmul

import scala.math.log
import scala.math.round

import chisel3._
import chisel3.util._


class MyMatmul extends Module {
  val io = IO(new Bundle {
    val row = Input(UInt(64.W))
    val col = Input(UInt(64.W))
    val result = Output(UInt(32.W))
  })

    //Row: 32 floating point numbers, each 32 bits
    //Col: 32 floating point numbers, each 32 bits

    //DMA: Get fed the starting address + 1023 (?) of the matrix then determine addresses of row and column elements from there
    //by partitioning at 32 bits

    //Input Matrix 1: A
    //Input Matrix 2: B
    //Output Matrix: C = AB
    val unpacked_row = io.row.asTypeOf(new MyRow)
    val unpacked_col = io.col.asTypeOf(new MyColumn)

    val prod1 = Module(new MyFloatOpp)
    prod1.io.opp := 1.U
    prod1.io.in0 := unpacked_row.a1_1
    prod1.io.in1 := unpacked_col.b1_1
    val prod1_result = prod1.io.out

    val prod2 = Module(new MyFloatOpp)
    prod2.io.opp := 1.U
    prod2.io.in0 := unpacked_row.a1_2
    prod2.io.in1 := unpacked_col.b2_1
    val prod2_result = prod2.io.out

    val sum = Module(new MyFloatOpp)
    sum.io.opp := 0.U
    sum.io.in0 := prod1_result
    sum.io.in1 := prod2_result
    val sum_result = sum.io.out

    io.result := sum_result
}