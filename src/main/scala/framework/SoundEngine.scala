package framework

import javax.sound.sampled._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.math.{max, min}


class SoundEngine(var source: SiGen) extends LineListener {

  var running=false

  // sample counter, needs to continue at same position when soundengine is stopped and restarted
  var sidCounter:Int = 0

  def start() {
    running = true
    Future{run()}

  }

  def run() {

    val bufferSize = 2048// 8 * 2048
    val format = new AudioFormat(44100.0f, 16, 2, true, true)

    val dataLineInfo:DataLine.Info  = new DataLine.Info(classOf[SourceDataLine], format);
    var sourceDataLine = AudioSystem.getLine(dataLineInfo).asInstanceOf[SourceDataLine];
    sourceDataLine.open(format, bufferSize);
    sourceDataLine.addLineListener(this)
    sourceDataLine.start();

    System.out.println("sourceDataLine buffer size:" + sourceDataLine.getBufferSize())
    Thread.currentThread().setPriority(Thread.MAX_PRIORITY)
    System.out.println("prio " + Thread.currentThread().getPriority() )
   System.out.println("Initial available samples in buffer: " + sourceDataLine.available)

    //int cnt = 0;
    val internalBufferSize = 512 // 1024
    //byte tempBuffer[] = new byte[1000];
    val tempBuffer = new Array[Byte](internalBufferSize)

    // somewhere I read there needs to be something in the buffer before starting the Line
    sourceDataLine.write(tempBuffer, 0, internalBufferSize);

    while(running) {
      for (i<-0 until internalBufferSize/4) {
        var valueL = source.getValue(sidCounter, 0)
        var valueR = source.getValue(sidCounter, 1)


        //if (i==0) System.out.println("Sample before conversion:" + value)
        sidCounter += 1

        // convert to byte, in 8 bit format
        //value = value * 128
        //tempBuffer(i) = value.toByte
        //if (i==0) System.out.println("Sample:" + valueL)

        if (sidCounter % 44100 == 0 )  {
          //System.out.println("avilable samples in buffer: " + sourceDataLine.available)
        }


        // convert to bytes, in 16 bit format
        valueL = 0.1f * valueL // reduce volume by factor 10 (each osc goes to +-1, but when we sum them we could go higher)
        valueR = 0.1f * valueR // reduce volume by factor 10 (each osc goes to +-1, but when we sum them we could go higher)
        val valueIntL = min(max((valueL * 32768),-32768), 32767).toInt // clipping
        val valueIntR = min(max((valueR * 32768),-32768), 32767).toInt // clipping

        tempBuffer(i*4)= (valueIntL >> 8).toByte
        tempBuffer(i*4+1)=  (valueIntL).toByte
        tempBuffer(i*4+2)=  (valueIntR >> 8).toByte
        tempBuffer(i*4+3)= (valueIntR).toByte

      }
      //if (sourceDataLine.available > bufferSize * 0.9) {
        //System.out.println("Now writing - 10% to underrun: " + sourceDataLine.available)
        //sourceDataLine.write(tempBuffer, 0, internalBufferSize);
        //System.out.println("After writing: " + sourceDataLine.available)
      //} else
      sourceDataLine.write(tempBuffer, 0, internalBufferSize)

    }

    sourceDataLine.drain();
    sourceDataLine.close();

  }

  def stop() {
    running = false
  }

  override def update(e: LineEvent) = {
    System.out.println("LineEvent"+ e)
  }

}
