package se.uu.it.jaba;

import com.ericsson.otp.erlang.*;

public class YabaReceiver implements Runnable {

	OtpMbox mbox;
	YabaConnection connection;
	
	public YabaReceiver(OtpMbox mbox, YabaConnection connection) {
		this.connection = connection;
		this.mbox = mbox;
	}
	
	@Override
	public void run() {
		OtpErlangObject object;
		
		while (true) {
			try {
				object = mbox.receive();
				
				OtpErlangTuple tuple = (OtpErlangTuple) object;
				OtpErlangAtom tag = (OtpErlangAtom) tuple.elementAt(0);
				OtpErlangTuple msgTuple = (OtpErlangTuple) tuple.elementAt(1);
				
				if (tag.toString().equals("message")) {					
					connection.routeMessage(msgTuple);
				} else {
					System.out.println("YabaReceiver: Received malformed message.");
				}
			} catch (OtpErlangExit e) {
				e.printStackTrace();
			} catch (OtpErlangDecodeException e) {
				e.printStackTrace();
			}
		}
	}
}
