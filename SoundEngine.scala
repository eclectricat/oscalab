import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;

import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioInputStream;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.DataLine;
import javax.sound.sampled.LineUnavailableException;
import javax.sound.sampled.SourceDataLine;
import javax.sound.sampled.TargetDataLine;

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import scala.math.max
import scala.math.min


class SoundEngine(var source: SiGen) {

  var running=false

  // sample counter, needs to continue at same position when soundengine is stopped and restarted
  var sidCounter:Int = 0

  def start() {

    running = true
    Future{run()}

  }

  def run() {

    val format = new AudioFormat(44100.0f, 16, 2, true, true)

    val dataLineInfo:DataLine.Info  = new DataLine.Info(classOf[SourceDataLine], format);
    var sourceDataLine = AudioSystem.getLine(dataLineInfo).asInstanceOf[SourceDataLine];
    sourceDataLine.open(format, 2048);
    sourceDataLine.start();

    System.out.println("sourceDataLine buffer size:" + sourceDataLine.getBufferSize())


    //int cnt = 0;
    val bufferSize = 1000
    //byte tempBuffer[] = new byte[1000];
    val tempBuffer = new Array[Byte](bufferSize)


    while(running) {
      for (i<-0 until bufferSize/4) {
        var valueL = source.getValue(sidCounter, 0)
        var valueR = source.getValue(sidCounter, 1)
        //if (i==0) System.out.println("Sample before conversion:" + value)
        sidCounter += 1

        // convert to byte, in 8 bit format
        //value = value * 128
        //tempBuffer(i) = value.toByte
        //if (i==0) System.out.println("Sample:" + valueL)

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
      sourceDataLine.write(tempBuffer, 0, bufferSize);
    }

    sourceDataLine.drain();
    sourceDataLine.close();

  }

  def stop() {
    running = false
  }

}
