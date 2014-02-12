import se.uu.it.jaba.YabaMessage;


public class TaxiMessage extends YabaMessage {

	private String name;
	private int x,y;
	
	public TaxiMessage(String id) {
		this.topic = "taxi";
		this.id = id;
	}
	
	public void setName(String name) {
		this.name = name;
	}
	
	public String getName() {
		return name;
	}
	
	public void setX(int x) {
		this.x = x;
	}
	
	public void setY(int y) {
		this.y = y;
	}
	
	public void setXY(int x, int y) {
		this.x = x;
		this.y = y;
	}
	
	public int getX() {
		return x;
	}
	
	public int getY() {
		return y;
	}
	
	public String toString() {
		return "TaxiMessage: " + id;
	}
}
