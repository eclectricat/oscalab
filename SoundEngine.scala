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

  def start() {

    running = true
    Future{run()}

  }

  def run() {

    val format = new AudioFormat(44100.0f, 16, 1, true, true)

    val dataLineInfo:DataLine.Info  = new DataLine.Info(classOf[SourceDataLine], format);
    var sourceDataLine = AudioSystem.getLine(dataLineInfo).asInstanceOf[SourceDataLine];
    sourceDataLine.open(format, 1024);
    sourceDataLine.start();

    System.out.println("sourceDataLine buffer size:" + sourceDataLine.getBufferSize())


    //int cnt = 0;
    val bufferSize = 1000
    //byte tempBuffer[] = new byte[1000];
    val tempBuffer = new Array[Byte](bufferSize)
    var sidCounter:Int = 0 // sample counter

    while(running) {
      for (i<-0 until bufferSize/2) {
        var value = source.getValue(sidCounter)
        //if (i==0) System.out.println("Sample before conversion:" + value)
        sidCounter += 1

        // convert to byte 8 bit
        //value = value * 128
        //tempBuffer(i) = value.toByte
        //if (i==0) System.out.println("Sample:" + value.toByte)

        // convert to bytes 16 bit
        value = 0.1f * value // reduce volume by factor 10 (each osc goes to +-1, but when we sum them we could go higher)
        val valueInt = min(max((value * 32768),-32768), 32767).toInt // clipping
        tempBuffer(i*2)= (valueInt >> 8).toByte
        tempBuffer(i*2+1)= (valueInt).toByte

      }
      sourceDataLine.write(tempBuffer, 0, 1000);
    }

    sourceDataLine.drain();
    sourceDataLine.close();

  }

  def stop() {
    running = false
  }
  
}
