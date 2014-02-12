package se.uu.it.jaba;

import java.util.Observable;
import java.util.Observer;

public class JabaDemo implements Observer {

	YabaConnection connection;
	
	public static void main(String args[]) {
		 JabaDemo demo = new JabaDemo();
		 demo.setup();
	}
	
	private void setup() {
		connection = new YabaConnection();
		TestTopic testTopic = new TestTopic();
		testTopic.addObserver(this);
		connection.subscribe(testTopic);
	}

	@Override
	public void update(Observable arg0, Object o) {
		TestMessage message = (TestMessage) o;
		System.out.println("Updated: " + message);
	}
}
