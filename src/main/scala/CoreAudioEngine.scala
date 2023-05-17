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
import javax.sound.sampled.LineListener;
import javax.sound.sampled.LineEvent;

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import scala.math.max
import scala.math.min

import ch.section6.jcoreaudio.AudioDevice
import ch.section6.jcoreaudio.JCoreAudio
import ch.section6.jcoreaudio.AudioLet
import ch.section6.jcoreaudio.CoreAudioState
import scala.collection.JavaConverters._

class CoreAudioEngine(var source: SiGen) extends ch.section6.jcoreaudio.CoreAudioListener {

  // sample counter, needs to continue at same position when soundengine is stopped and restarted
  var sidCounter:Int = 0

  val audioDeviceList = JCoreAudio.getAudioDeviceList().asScala.toList

  audioDeviceList.map{d => System.out.println(d)}

  val inputSet = audioDeviceList(0).getInputSet();
  val outputDevice = audioDeviceList(1)
  val outputSet = outputDevice.getOutputSet();

  // Configure JCoreAudio with the input and output let sets (one or the other may also be null.
  // A block size of 512 is used, along with the current sample rate. The current sample rate is
  // the same as that reported by the Audio MIDI Setup application.
  JCoreAudio.getInstance().initialize(
    inputSet, outputSet,
    outputDevice.getCurrentBufferSize(),
    outputDevice.getCurrentSampleRate())

    JCoreAudio.getInstance().setListener(this)


    def start() {

      JCoreAudio.getInstance().play();

    }

    def stop() {
      //running = false
      JCoreAudio.getInstance().returnToState(CoreAudioState.UNINITIALIZED);
    }

    def onCoreAudioInput(timestamp: Double,audioLets: java.util.Set[ch.section6.jcoreaudio.AudioLet]): Unit = {

    }
    def onCoreAudioOutput(timestamp: Double, outputLets: java.util.Set[ch.section6.jcoreaudio.AudioLet]): Unit = {
      var blockSize:Int = 512;

      /**
      val temBuf = Array[Float](blockSize)

      val toIdx = sidCounter + blockSize
      //for(var j <- sidCounter to toIdx) {
      var tempIdx = 0
      while (sidCounter < toIdx) {
        val value = math.sin(2.0 * Math.PI * sidCounter * 440.0 / 44100.0).toFloat
        //buffers(0).put(value);
        //buffers(1).put(value);
        temBuf(tempIdx) = value
        tempIdx += 1
        sidCounter += 1
      }
      */

      val let:AudioLet = outputLets.iterator().next()
      var buffers = List[java.nio.FloatBuffer]()
      for (i <- 0 until let.numChannels) {
        val buffer = let.getChannelFloatBuffer(i);
        buffer.rewind();
        blockSize = buffer.capacity()
        buffers = buffers ++ List(buffer)
      }

        for (j <- 0 until blockSize) {
          buffers(0).put(source.getValue(sidCounter, 0))
          buffers(1).put(source.getValue(sidCounter, 0))
          sidCounter += 1
        }
    }

}
