package se.uu.it.jaba;

import java.util.HashMap;

import com.ericsson.otp.erlang.*;

public class TestTopic extends YabaTopic {

	public TestTopic() {
		this.name = "test";
		messages = new HashMap<String, TestMessage>();
	}
	
	@Override
	public void parseMessage(OtpErlangTuple tuple) {
		OtpErlangLong id = (OtpErlangLong) tuple.elementAt(0);
		// OtpErlangAtom topic = (OtpErlangAtom) tuple.elementAt(1);
		// OtpErlangTuple timestamp = (OtpErlangTuple) tuple.elementAt(2);
		OtpErlangString body = (OtpErlangString) tuple.elementAt(3);
		
		TestMessage message = new TestMessage(id.toString());
		message.setBody(body.toString());		
		
		messages.put(id.toString(), message);
		// System.out.println(messages.get(id.toString()));
		
		setChanged();
		notifyObservers(message);
	}
}
