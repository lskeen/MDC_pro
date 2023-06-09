package matmul

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import chisel3.experimental.BundleLiterals._
import scala.language.postfixOps

class MyMatmulTest extends AnyFreeSpec with ChiselScalatestTester {

    "Starting Test Case" in {
        test (new MyMatmul) { c =>

            c.io.row.poke("b0011111110000101000111101011100001000000001001110000101000111101".U)
            c.io.col.poke("b0100000001000111101011100001010001000000000111001100110011001101".U)
            c.clock.step(1)
            c.io.result.expect("b01000001000110100011101010010010".U)

        

        }
    }
}