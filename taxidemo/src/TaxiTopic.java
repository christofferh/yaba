import java.util.HashMap;
import com.ericsson.otp.erlang.*;
import se.uu.it.jaba.YabaTopic;


public class TaxiTopic extends YabaTopic {
	
	public TaxiTopic() {
		this.name = "taxi";
		messages = new HashMap<String, TaxiMessage>();
	}
	
	@Override
	public void parseMessage(OtpErlangTuple tuple) {
		OtpErlangLong id = (OtpErlangLong) tuple.elementAt(0);
		// OtpErlangAtom topic = (OtpErlangAtom) tuple.elementAt(1);
		// OtpErlangTuple timestamp = (OtpErlangTuple) tuple.elementAt(2);
		OtpErlangTuple dataTuple = (OtpErlangTuple) tuple.elementAt(3);
		
		OtpErlangString taxiName = (OtpErlangString) dataTuple.elementAt(0);
		OtpErlangLong taxiXCoord = (OtpErlangLong) dataTuple.elementAt(1);
		OtpErlangLong taxiYCoord = (OtpErlangLong) dataTuple.elementAt(2);
		
		TaxiMessage message = new TaxiMessage(id.toString());
		try {
			message.setX(taxiXCoord.intValue());
		} catch (OtpErlangRangeException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		try {
			message.setY(taxiYCoord.intValue());
		} catch (OtpErlangRangeException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		message.setName(taxiName.toString());
		
		messages.put(id.toString(), message);
		
		setChanged();
		notifyObservers(message);
	}
}
