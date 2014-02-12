import se.uu.it.jaba.*;
import java.util.Observable;
import java.util.Observer;

public class TaxiDemo {
	
	TaxiFrame frame;
	
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		TaxiDemo demo = new TaxiDemo();
		demo.start();
	}
	
	private void start() {	
		frame = new TaxiFrame();
		frame.setVisible(true);
	}
}
