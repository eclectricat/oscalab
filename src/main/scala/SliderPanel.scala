import javax.swing._;
import java.awt.{List => _, _};
import javax.swing.event._;
import java.awt.event._;

trait SliderPanelElement {
  def draw(panel: JPanel)
}

class ParamInfo(val name:String, val min:Float, val max:Float, val value:ConstantValue) extends SliderPanelElement {
  def draw(panel: JPanel) = {
    val sliderLabel = new JLabel(name, SwingConstants.CENTER)
    sliderLabel.setAlignmentX(Component.CENTER_ALIGNMENT)

    val scaledValue = 100 * (value.value - min) / (max-min)
    val slider = new JSlider(SwingConstants.HORIZONTAL,0, 100, scaledValue.round)

    panel.add(sliderLabel)
    panel.add(slider)

    slider.addChangeListener(new SliderChangeListener(this))
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

}
