package se.uu.it.jaba;

import java.util.Map;
import java.util.Observable;
import java.util.Observer;

import com.ericsson.otp.erlang.*;

public abstract class YabaTopic extends Observable {

	protected String name;
	protected Map messages;
	
	public String getName() {
		return name;
	}
	
	public void parseMessage(OtpErlangTuple messageToParse) {
		
	}

}
