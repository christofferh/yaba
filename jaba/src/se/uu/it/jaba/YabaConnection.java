package se.uu.it.jaba;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.net.InetAddress;
import java.net.UnknownHostException;

import com.ericsson.otp.erlang.*;

public class YabaConnection {

	String nodeAddress;
	OtpNode self;
	OtpMbox mbox;
	Map<String, YabaTopic> topics;
	Thread receiverThread;
	
	public YabaConnection() {
		try {
			self = new OtpNode("jaba@localhost", "42");
		} catch (IOException e) {
			System.err.println("Unable to setup self node. Is epmd running?");
			e.printStackTrace();
			System.exit(1);
		}
		
		InetAddress inetAddress;
		try {
			inetAddress = InetAddress.getLocalHost();
			nodeAddress = "yaba@" + inetAddress.getCanonicalHostName();
		} catch (UnknownHostException e1) {
			System.err.println("Unable to find out FQDN to our own host.");
			e1.printStackTrace();
		}
		
		if (self.ping(nodeAddress, 2000)) {
			System.out.println("Pinged node.");
		} else {
			System.out.println("No connection to node.");
		}
		
		
		mbox = self.createMbox("mbox");
		topics = new HashMap<String, YabaTopic>();
		
		receiverThread = new Thread(new YabaReceiver(mbox, this));
		receiverThread.start();
	}
	
	public void subscribe(YabaTopic topic) {
		topics.put(topic.getName(), topic);
	}
	
	public void routeMessage(OtpErlangTuple msgTuple) {
		OtpErlangAtom topic = (OtpErlangAtom) msgTuple.elementAt(1);
		
		if (topics.containsKey(topic.toString())) {
			YabaTopic topicToUse = topics.get(topic.toString());
			topicToUse.parseMessage(msgTuple);
		} else {
			System.out.println("Not subscribing to topic, throwing message.");
		}
	}

}
