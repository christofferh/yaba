package se.uu.it.jaba;

public class TestMessage extends YabaMessage {

	private String body;
	
	public TestMessage(String id) {
		this.topic = "test";
		this.id = id;
	}
	
	public void setBody(String body) {
		this.body = body;
	}
	
	public String getBody() {
		return body;
	}
	
	public String toString() {
		return "TestMessage: " + id;
	}
}
