import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.event.*;
import java.util.HashMap;
import java.util.Map;
import java.util.Observable;
import java.util.Observer;

import javax.swing.*;

import se.uu.it.jaba.*;

public class TaxiFrame extends JFrame {

	Container container;
	JPanel bgPanel;
	YabaConnection conn;
	
	Vehicle taxi1, taxi2, taxi3, taxi4;
	
	JButton taxi1Button, taxi2Button, taxi3Button, taxi4Button;

	
	public TaxiFrame() {		
		setTitle("Sambandscentralen");
		setDefaultCloseOperation(EXIT_ON_CLOSE);
		setSize(800,600);
		setResizable(false);
		
		container = getContentPane();

		container.setLayout(new BorderLayout());
		
		taxi1Button = new JButton("Taxi 1");
		taxi1Button.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent arg0) {
				if (taxi1.isVisible()) {
					taxi1.setVisible(false);
				} else {
					taxi1.setVisible(true);
				}
			}
		});
		taxi2Button = new JButton("Taxi 2");
		taxi2Button.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent arg0) {
				if (taxi2.isVisible()) {
					taxi2.setVisible(false);
				} else {
					taxi2.setVisible(true);
				}
			}
		});
		taxi3Button = new JButton("Taxi 3");
		taxi3Button.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent arg0) {
				if (taxi3.isVisible()) {
					taxi3.setVisible(false);
				} else {
					taxi3.setVisible(true);
				}
			}
		});
		taxi4Button = new JButton("Taxi 4");
		taxi4Button.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent arg0) {
				if (taxi4.isVisible()) {
					taxi4.setVisible(false);
				} else {
					taxi4.setVisible(true);
				}
			}
		});
		
		JPanel buttonPanel = new JPanel();
		buttonPanel.add(taxi1Button);
		buttonPanel.add(taxi2Button);
		buttonPanel.add(taxi3Button);
		buttonPanel.add(taxi4Button);
		container.add(buttonPanel,BorderLayout.PAGE_END);
		
        bgPanel = new JPanel() {
            public void paintComponent(Graphics g) {
                Image img = new ImageIcon("yabamap.jpg").getImage();
                Dimension size = new Dimension(img.getWidth(null), img.getHeight(null));
                setPreferredSize(size);
                setMinimumSize(size);
                setMaximumSize(size);
                setSize(size);
                setLayout(null);
                g.drawImage(img, 0, 0, null);
            } 
        };
        
        container.add(bgPanel);
        bgPanel.setBounds(0,0, 800, 600);
        bgPanel.addMouseListener(new PlotListener());
        
        conn = new YabaConnection();
        
        TaxiTopic taxiTopic = new TaxiTopic();
        conn.subscribe(taxiTopic);
        taxiTopic.addObserver(new TaxiObserver());
        
        taxi1 = new Vehicle("red.png");
        container.add(taxi1, 0);
        taxi1.move(10, 200);
        taxi1.setVisible(false);
        
        taxi2 = new Vehicle("green.png");
        container.add(taxi2, 0);
        taxi2.move(30, 200);
        taxi2.setVisible(false);
        
        taxi3 = new Vehicle("blue.png");
        container.add(taxi3, 0);
        taxi3.move(50, 200);
        taxi3.setVisible(false);
        
        taxi4 = new Vehicle("yellow.png");
        container.add(taxi4, 0);
        taxi4.move(70, 200);
        taxi4.setVisible(false);
	}
	
	class TaxiObserver implements Observer {
		@Override
		public void update(Observable arg0, Object object) {
			TaxiMessage message = (TaxiMessage) object;
			String id = message.getId();
			
			if (id.equals("1")) {
				taxi1.move(message.getX(), message.getY());
			} else if (id.equals("2")) {
				taxi2.move(message.getX(), message.getY());
			} else if (id.equals("3")) {
				taxi3.move(message.getX(), message.getY());
			} else if (id.equals("4")) {
				taxi4.move(message.getX(), message.getY());
			}
		}
	}
	
	class PlotListener extends MouseAdapter {
		public void mouseClicked(MouseEvent e) {
			System.out.println(e.getX() + " " + e.getY());
			taxi1.move(e.getX(), e.getY());
		}
	}
}
