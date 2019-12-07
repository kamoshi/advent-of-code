package day03;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.Scanner;


/**
 * 
 * @author kam
 * 
 * Solution to task 2
 *
 */
public class Claim {
	
	boolean overlapped;
	
	int id;
	int marginleft;
	int margintop;
	int width;
	int height;
	
	public Claim(int id, int marginleft, int margintop, int width, int height) {
		this.id = id;
		this.marginleft = marginleft;
		this.margintop = margintop;
		this.width = width;
		this.height = height;
	}
	
	public void assignClaim(Claim[][] canvas) {
		for (int i=0; i < width; i++) {
			for (int j=0; j < height; j++) {
				if (canvas[marginleft + i][margintop + j] == null) {
					canvas[marginleft + i][margintop + j] = this;
				} else {
					canvas[marginleft + i][margintop + j].overlapped = true;
					this.overlapped = true;
				}
			}
		}
	}
	
	
	public static void main(String[] args) {
		
		ArrayList<Claim> array = new ArrayList<>();
		Claim[][] canvas = new Claim[1000][1000];
		
		try 
		{
			Scanner scanner = new Scanner(new BufferedReader(new FileReader(new File("src/day03/input.txt"))));
			
			while(scanner.hasNextLine()) {
				String s = scanner.nextLine();
				
				int div1 = s.indexOf('@');
				int div2 = s.indexOf(',');
				int div3 = s.indexOf(':');
				int div4 = s.indexOf('x');
				
				int id = Integer.parseInt(s.substring(1, div1).replaceAll("\\s",""));
				int coordx = Integer.parseInt(s.substring(div1+1, div2).replaceAll("\\s",""));
				int coordy = Integer.parseInt(s.substring(div2+1, div3).replaceAll("\\s",""));
				int height = Integer.parseInt(s.substring(div3+1, div4).replaceAll("\\s",""));
				int length = Integer.parseInt(s.substring(div4+1).replaceAll("\\s",""));
				
				Claim generatedClaim = new Claim(id, coordx, coordy, height, length);
				generatedClaim.assignClaim(canvas);
				array.add(generatedClaim);
				
			}
			scanner.close(); // close stream
			
		}
		catch (Exception e) {
			System.out.println(e.toString() + " : " + e.getStackTrace().toString());
		}
		
		for(Claim claim : array) {
			if (claim.overlapped == false) {
				System.out.println("Not overlapped: " + claim.id);
			}
		}
	}
}
