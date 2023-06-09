package matmul

import chisel3._

class MyColumn extends Bundle {
    val b1_1 = UInt(32.W)
    val b2_1 = UInt(32.W)
}