package framework

class Stutter(in: SiGen, val loopStart:SiGen, val loopEnd:SiGen, val enable:SiGen, val move:SiGen, bpm: SiGen) extends SiGen with Triggerable {

  val bufferLength = 10240
  val samples = new Array[Float](bufferLength)

  var loopingActive = false
  var writehead:Int= 0;
  var  playhead:Int = 0;
  var driftOffset:Int = 0;
  var direction:Int= 1;


  override def getValue(sid: Int): Float  =  {
    val inVal = in.getValue(sid)

    if (enable.getValue(sid) < 0.5f) {
      loopingActive = false;
      return inVal
    }

    val currentBpm = bpm.getValue(sid);

    // Calculate samples in one 16th note (60/BPM/4 * 44100)
    val stepSamples =Math.min( bufferLength, (60.0f / (currentBpm * 4.0f)) * 44100.0f).toInt ;
    val mVal = move.getValue(sid);

    // Calculate base indices and apply drift
    val baseStart = ((loopStart.getValue(sid) * (stepSamples - 1))).toInt;
    val baseEnd = (loopEnd.getValue(sid) * (stepSamples - 1)).toInt;

    // Wrap indices within the 1/16th note range
    var startIdx = (baseStart + driftOffset.toInt) % stepSamples.toInt;
    var endIdx = (baseEnd + driftOffset.toInt) % stepSamples.toInt;

    if (startIdx < 0) startIdx += stepSamples.toInt;
    if (endIdx < 0) endIdx += stepSamples.toInt;

    // Capture input into the buffer (one step only)
    if (writehead < stepSamples) {
      samples(writehead) = inVal;
      writehead+=1;
    }

    var value = inVal
    if (loopingActive) {
      value = samples(playhead);
      playhead += direction;

      if (direction == 1) {
        if (startIdx < endIdx) {
          // Normal forward loop
          if (playhead >= endIdx) {
            playhead = startIdx;
            driftOffset += ( mVal * (endIdx - startIdx)).toInt;
          }
        } else {
          // Reverse mode: play forward until startIdx, then reverse
          if (playhead >= startIdx) {
            direction = -1;
            playhead = startIdx;
          }
        }
      } else {
        // direction == -1: Backwards loop
        if (playhead <= endIdx) {
          playhead = startIdx;
          direction = 1;
          driftOffset += ( mVal * (startIdx - endIdx)).toInt;
        }
      }

      // Final safety bounds
      if (playhead < 0) playhead = 0;
      if (playhead >= bufferLength) playhead = bufferLength - 1;

      // Keep driftOffset wrapped within the capture buffer
      if (driftOffset >= stepSamples) driftOffset -= stepSamples;
      if (driftOffset < -stepSamples) driftOffset += stepSamples;
    } else {
      value = inVal;
    }

    return value
  }

  def retrigger()  = {
    if (enable.getValue(0) > 0.5f) {
      writehead = 0;
      playhead = 0;
      driftOffset = 0;
      direction = 1;
      loopingActive = true;
    }
  }

  def release()  = {
    // stop looping, continue playing the last sample of the buffer, which contains the input signal
    loopingActive = false;
  }



}

