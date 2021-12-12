import javax.swing._;
import java.awt.{List => _, _};
import javax.swing.event._;
import java.awt.event._;

class ParamInfo(val name:String, val min:Float, val max:Float, val value:ConstantValue)

class SliderChangeListener(param: ParamInfo) extends ChangeListener {
  def stateChanged(event: ChangeEvent) = {
    val sliderValue = event.getSource().asInstanceOf[JSlider].getValue
    val realValue = (sliderValue / 100.0) * (param.max-param.min) + param.min

    param.value.value = realValue.toFloat
    System.out.println("Value changed, slider:" +sliderValue)
    System.out.println("Value changed, param :" +param.value.value)
  }
}

class SliderPanel(params:List[ParamInfo]) {

  var frame:Option[JFrame] = None

  def show() = {
    val f:JFrame = new JFrame("AdjustableParameters");
    //frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    //SliderDemo animator = new SliderDemo();
    val panel = new JPanel()
    panel.setLayout(new BoxLayout(panel, BoxLayout.PAGE_AXIS));

    params.map { param =>
      val sliderLabel = new JLabel(param.name, SwingConstants.CENTER);
      sliderLabel.setAlignmentX(Component.CENTER_ALIGNMENT);

      val scaledValue = 100 * (param.value.value - param.min) / (param.max-param.min)
      val slider = new JSlider(SwingConstants.HORIZONTAL,0, 100, scaledValue.round)
      f.add(panel)
      panel.add(sliderLabel)
      panel.add(slider)

      slider.addChangeListener(new SliderChangeListener(param))
    }


    //framesPerSecond.addChangeListener(this);

    //Add content to the window.
    //frame.add(animator, BorderLayout.CENTER);

    //Display the window.
    f.pack();
    f.setVisible(true);
    frame = Some(f)
    //animator.startAnimation();
    //return f
  }

  def close() = {frame.map(_.setVisible(false))}

}
