import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Image;

import javax.swing.ImageIcon;
import javax.swing.JPanel;

public class Vehicle extends JPanel {
	
	Image image;
	Dimension size;
	String id;
	
	public Vehicle(String filename) {
		image = new ImageIcon(filename).getImage();
		size = new Dimension(image.getWidth(null), image.getHeight(null));
        setPreferredSize(size);
        setMinimumSize(size);
        setMaximumSize(size);
        setSize(size);
        setLayout(null);
        move(0,0);
	}
	
	public void setId(String id) {
		this.id = id;
	}
	
	public String getId() {
		return id;
	}
	
	public void move(int x, int y) {
		setBounds(x - size.width / 4, y - size.height + 3 , size.width, size.height);
	}
	
    public void paintComponent(Graphics g) {
    	g.drawImage(image, 0, 0, null);
    }
}
