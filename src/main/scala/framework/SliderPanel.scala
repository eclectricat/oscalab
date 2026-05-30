package framework

import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.native.JsonMethods._

import java.awt.{List => _, _}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import javax.swing._
import javax.swing.event._
import scala.io.Source

trait SliderPanelElement {
  def draw(panel: JPanel)
  def jsonRepr: Option[(String, org.json4s.JDouble)] = None
  def set(newValue: Float) = { }
}

class ParamInfo(val name:String, val min:Float, val max:Float, val value:ConstantValue) extends SliderPanelElement {

  var slider:Option[JSlider] = None

  def draw(panel: JPanel) = {
    val sliderLabel = new JLabel(name, SwingConstants.CENTER)
    sliderLabel.setAlignmentX(Component.CENTER_ALIGNMENT)

    val scaledValue = 100 * (value.value - min) / (max-min)
    val sl = new JSlider(SwingConstants.HORIZONTAL,0, 100, scaledValue.round)
    slider = Some(sl)

    panel.add(sliderLabel)
    panel.add(sl)

    sl.addChangeListener(new SliderChangeListener(this))
  }

  override def jsonRepr: Option[(String, org.json4s.JDouble)] = Some((name, JDouble(value.value)))
  override def set(newValue: Float) = {
    value.value = newValue

    val intValue = (100 * (value.value - min) / (max-min))
    slider.map(_.setValue(intValue.round))
  }

}

class PanelDividerUI(val text:String) extends SliderPanelElement {
  def draw(panel: JPanel) = {
    val fulltext = "<html><p style=\"padding: 5px; border: 1px solid black;\">" + text + "</p></html>"
    val sliderLabel = new JLabel(fulltext, SwingConstants.CENTER)
    sliderLabel.setAlignmentX(Component.CENTER_ALIGNMENT)
    panel.add(sliderLabel)
  }
}

class SliderChangeListener(param: ParamInfo) extends ChangeListener {
  def stateChanged(event: ChangeEvent) = {
    val sliderValue = event.getSource().asInstanceOf[JSlider].getValue
    val realValue = (sliderValue / 100.0) * (param.max-param.min) + param.min

    param.value.value = realValue.toFloat
    System.out.println("Value changed, slider:" +sliderValue)
    System.out.println("Value changed, param :" +param.value.value)
  }
}

class SliderPanel(params:List[SliderPanelElement]) {

  var frame:Option[JFrame] = None

  def show() = {
    val f:JFrame = new JFrame("AdjustableParameters");

    val panel = new JPanel()
    panel.setLayout(new BoxLayout(panel, BoxLayout.PAGE_AXIS));

    params.map { param => param.draw(panel) }

    f.add(panel)

    //Display the window.
    f.pack();
    f.setVisible(true);
    frame = Some(f)
  }

  def close() = {frame.map(_.setVisible(false))}

  def saveParams(filename: Option[String]) = {
    val json = render(params.map(p => p.jsonRepr).flatten)
    val cjson = compact(json)

    filename.map { fn =>
      Files.write(Paths.get(fn), cjson.getBytes(StandardCharsets.UTF_8))
    }
    cjson
  }

  def loadParamsFromFile(filename:String) = {
    val string = Source.fromFile(filename).mkString
    loadParams(string)
    string
  }

  def loadParams(json:String) = {
    val map = parse(json).values.asInstanceOf[Map[String, Double]]

    params.foreach {
      case p: ParamInfo =>
        map.get(p.name) match {
          case Some(double) => {
             p.set(double.toFloat)
          }
          case _ => System.out.println("parameter not found: "+p.name)
        }
      case _ => System.out.println("sliderpanelelement that is not a paraminfo")
    }
  }

}
