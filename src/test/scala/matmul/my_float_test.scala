package matmul

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import chisel3.experimental.BundleLiterals._
import scala.language.postfixOps

class MyFloatTest extends AnyFreeSpec with ChiselScalatestTester {

    "Starting Test Case" in {
        test(new MyFloatOpp) { c =>
            //                              ADDITION TESTS:
            c.io.in0.poke("b01000010111110100000000000000000".U)    
            c.io.in1.poke("b01000000101100000000000000000000".U)
            c.io.opp.poke("b0".U)
            c.clock.step(1)
            c.io.out.expect("b01000011000000101000000000000000".U)

            c.io.in0.poke("b01010101101111000011001000101000".U)    
            c.io.in1.poke("b01010001011111111100010111010110".U)
            c.io.opp.poke("b0".U)
            c.clock.step(1)
            c.io.out.expect("b01010101101111001011001000001011".U)

            c.io.in0.poke("b00000110101110010100010001111111".U)    
            c.io.in1.poke("b00111001010100011011011100010111".U)
            c.io.opp.poke("b0".U)
            c.clock.step(1)
            c.io.out.expect("b00111001010100011011011100010111".U)

            c.io.in0.poke("b01111111011111111001111100000000".U)    
            c.io.in1.poke("b01111101011111111100010111010110".U)
            c.io.opp.poke("b0".U)
            c.clock.step(1)
            c.io.out.expect("b01111111100000000000000000000000".U)

            c.io.in0.poke("b00000000011100001000100001111111".U)    
            c.io.in1.poke("b00001110110010000011100001111011".U)
            c.io.opp.poke("b0".U)
            c.clock.step(1)
            c.io.out.expect("b00001110110010000011100001111011".U)

            c.io.in0.poke("b00000000010010000011100001111011".U)    
            c.io.in1.poke("b00000000011100001000100001111111".U)
            c.io.opp.poke("b0".U)
            c.clock.step(1)
            c.io.out.expect("b00000000101110001100000011111010".U)

            c.io.in0.poke("b00000000010010000011100001111011".U)    
            c.io.in1.poke("b00000000000000001000100001111111".U)
            c.io.opp.poke("b0".U)
            c.clock.step(1)
            c.io.out.expect("b00000000010010001100000011111010".U)



            //                             SUBTRACTION TESTS:
            c.io.in0.poke("b00111111100000000000000000000000".U)    
            c.io.in1.poke("b10111111011111110000000000000000".U)
            c.io.opp.poke("b0".U)
            c.clock.step(1)
            c.io.out.expect("b00111011100000000000000000000000".U)

            c.io.in0.poke("b00000000010010000011100001111011".U)    
            c.io.in1.poke("b10000000000000001000100001111111".U)
            c.io.opp.poke("b0".U)
            c.clock.step(1)
            c.io.out.expect("b00000000010001111010111111111100".U)

            c.io.in0.poke("b11111000010010000011100001111011".U)    
            c.io.in1.poke("b00011100000000001000100001111111".U)
            c.io.opp.poke("b0".U)
            c.clock.step(1)
            c.io.out.expect("b11111000010010000011100001111011".U)

            c.io.in0.poke("b11111111011111111111111111111111".U)    
            c.io.in1.poke("b00011100000000001000100001111111".U)
            c.io.opp.poke("b0".U)
            c.clock.step(1)
            c.io.out.expect("b11111111011111111111111111111111".U)

            c.io.in0.poke("b11000011100101100000000000000000".U)    
            c.io.in1.poke("b01000011111110100000000000000000".U)
            c.io.opp.poke("b0".U)
            c.clock.step(1)
            c.io.out.expect("b01000011010010000000000000000000".U)

            c.io.in0.poke("b11001010011010101110011100101100".U)    
            c.io.in1.poke("b01001110101001001110001001110011".U)
            c.io.opp.poke("b0".U)
            c.clock.step(1)
            c.io.out.expect("b01001110101001000110110011111111".U)

            //                              MULTIPLICATION TESTS:
            c.io.in0.poke("b01001101111101000100110111010001".U)    
            c.io.in1.poke("b10101010101010101010101010101010".U)
            c.io.opp.poke("b1".U)
            c.clock.step(1)
            c.io.out.expect("b10111001001000101101111010001011".U)

            c.io.in0.poke("b10101010101010101010101010101010".U)    
            c.io.in1.poke("b10101010101010101010101010101010".U)
            c.io.opp.poke("b1".U)
            c.clock.step(1)
            c.io.out.expect("b00010101111000111000111000110111".U)

            c.io.in0.poke("b01111111000000000000000000000000".U)    
            c.io.in1.poke("b01000001101000000000000000000000".U)
            c.io.opp.poke("b1".U)
            c.clock.step(1)
            c.io.out.expect("b01111111100000000000000000000000".U)

            c.io.in0.poke("b00000000101110010100010001110000".U)    
            c.io.in1.poke("b10110000000010010111000001011111".U)
            c.io.opp.poke("b1".U)
            c.clock.step(1)
            c.io.out.expect("b10000000000000000000000000000000".U)

            c.io.in0.poke("b00000000101110010100010001110000".U)    
            c.io.in1.poke("b10111000010100011011011100010111".U)
            c.io.opp.poke("b1".U)
            c.clock.step(1)
            c.io.out.expect("b10000000000000000000001001011111".U)

            c.io.in0.poke("b01000000000000000000000000000000".U)    
            c.io.in1.poke("b00111111000000000000000000000000".U)
            c.io.opp.poke("b1".U)
            c.clock.step(1)
            c.io.out.expect("b00111111100000000000000000000000".U)

            c.io.in0.poke("b00000110101110010100010001111111".U)    
            c.io.in1.poke("b10111001010100011011011100010111".U)
            c.io.opp.poke("b1".U)
            c.clock.step(1)
            c.io.out.expect("b10000000100101111100010101101100".U)

            c.io.in0.poke("b00000100101110010100010001111111".U)    
            c.io.in1.poke("b10111001011100001011011100010111".U)
            c.io.opp.poke("b1".U)
            c.clock.step(1)
            c.io.out.expect("b10000000000010101110001101001011".U)

            c.io.in0.poke("b01100000000000000000000000000000".U)       // MULT: Denorm input -> Norm output (Works)
            c.io.in1.poke("b00000000000100000000000000000000".U)
            c.io.opp.poke("b1".U)
            c.clock.step(1)
            c.io.out.expect("b00011111100000000000000000000000".U)

            c.io.in0.poke("b01000000000000000000000000000000".U)       // MULT: Denorm input -> Norm output (Works)
            c.io.in1.poke("b00000000010000000000000000000000".U)       // (Close to denorm out)
            c.io.opp.poke("b1".U)
            c.clock.step(1)
            c.io.out.expect("b00000000100000000000000000000000".U)

            c.io.in0.poke("b01000000000000000000000000000000".U)       // MULT: Denorm input -> DeNorm output 
            c.io.in1.poke("b00000000001000000000000000000000".U)       // (Close to Norm out)
            c.io.opp.poke("b1".U)
            c.clock.step(1)
            c.io.out.expect("b00000000010000000000000000000000".U)

            c.io.in0.poke("b00111111000000000000000000010011".U)       // MULT: Denorm input -> DeNorm output 
            c.io.in1.poke("b00000000010000000110000000000111".U)       
            c.io.opp.poke("b1".U)
            c.clock.step(1)
            c.io.out.expect("b00000000001000000011000000001000".U)

            c.io.in0.poke("b10000000101110010100010001111111".U)       
            c.io.in1.poke("b00111001011100001011011101111111".U)       
            c.io.opp.poke("b1".U)
            c.clock.step(1)
            c.io.out.expect("b10000000000000000000101011100011".U)  
        
            // println(c.io.out.peek())
        }
    }
}