package se.uu.it.jaba;

import java.sql.Timestamp;

public abstract class YabaMessage {

	protected String topic;
	protected String id;
	protected Timestamp timestamp;
	
	public String getTopic() {
		return topic;
	}
	
	public String getId() {
		return id;
	}
	
	public Timestamp getTimestamp() {
		return timestamp;
	}
	
	@Override
	public String toString() {
		return "YabaMessage: " + id;
	}
}
