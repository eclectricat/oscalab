import javax.swing._;
import java.awt.{List => _, _};
import javax.swing.event._;
import java.awt.event._;


class DrawablePanel {

  var dPanel: Option[JPanel] = None
  init()


  def init():Unit = {
    System.out.println("Hi there")

    val jFrame = new JFrame()

    val dPanel = new DrawingArea(this)
    this.dPanel = Some(dPanel)
    dPanel.setPreferredSize(new Dimension(800,400))
    dPanel.addMouseListener(new MouseAdapter() {
      override def mousePressed(ev: MouseEvent) = mouseClick(ev)
      override def mouseReleased(ev: MouseEvent) = mouseRelease(ev)
    })

    jFrame.add(dPanel)
    jFrame.pack()
    jFrame.setVisible(true)
  }

  def mouseClick(ev: MouseEvent) = {
    System.out.println("Click")
  }

  def mouseRelease(ev: MouseEvent) = {

  }

  def redraw() = { // when content changed
    System.out.println("redrawing...")
    //dPanel.map(_.revalidate())
    //dPanel.map(_.invalidate())
    // TODO: there must be a better way to do this
    dPanel.map(_.setVisible(false))
    dPanel.map(_.setVisible(true))

  }

  def paintComponent(g: Graphics) = {
    System.out.println("drawing")
    g.drawString("CIAO", 20,20)
  }

  class DrawingArea(d: DrawablePanel) extends JPanel {
    override def paintComponent(g: Graphics) = {
      super.paintComponent(g)
      d.paintComponent(g)
    }
  }

}

class WaveDisp(val source: SiGen, val length: Float) extends DrawablePanel {

  var samples: Option[List[Float]] = None
  calculate()

  redraw()

  def calculate() = {

    val numSamples = (length * GlobalConfig.sampleRate).floor.toInt
    val calculatedSamples = (0 to numSamples).toList.map(source.getValue(_))

    System.out.println("number of samples: " + calculatedSamples.size)
    val maxNum = 800
    if (calculatedSamples.size > maxNum) {
      val subsampled = (0 until maxNum).map { i =>
        val index = i * (calculatedSamples.size/maxNum.toFloat)
        calculatedSamples(index.round)
      }
      samples = Some(subsampled.toList)
    } else
      samples = Some(calculatedSamples)
    System.out.println("number of samples: " + samples.get.size)
    System.out.println("calculated")
  }

  override def paintComponent(g : Graphics) = { (dPanel, samples) match {

    case (Some(dPanel), Some(samples)) =>

    val yFactor = 0.5f * dPanel.getHeight() * 0.5f
    val yOffset = dPanel.getHeight()/2
    val deltaX = dPanel.getWidth().toFloat / samples.size
    var xPos = 0f

    var prevPosition = (0, yOffset - (samples(0) * yFactor).round)
    samples.map { sample =>
      val newPosition = (xPos.toInt, yOffset - (sample * yFactor).round)
      //g.drawOval(xPos.toInt, yOffset + (sample * yFactor).round , 2, 2);
      g.drawLine(prevPosition._1, prevPosition._2, newPosition._1, newPosition._2)
      prevPosition = newPosition
      xPos = xPos + deltaX
    }

    case _ => System.out.println("Not ready: "+ (dPanel, samples))
  }
}

}
