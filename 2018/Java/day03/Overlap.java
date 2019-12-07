package day03;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.Scanner;

public class Overlap {
	
	int[][] canvas = new int[1000][1000];
	
	public String parseFile(File file) {
		try 
		{
			Scanner scanner = new Scanner(new BufferedReader(new FileReader(file)));
			
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
				
				System.out.println("" + id + "|" + coordx + "|" + coordy + "|" + height + "|" + length);
				
				for (int i = 0; i < height; i++) {
					for (int j = 0; j < length; j++) {
						canvas[coordx + i][coordy + j]++;
					}
				}
				
			}
			scanner.close(); // close stream
			
			int overlapped = 0;
			
			for (int i = 0; i < canvas.length; i++) {
				for (int j = 0; j < canvas[0].length; j++) {
					if (canvas[i][j] > 1) {
						overlapped++;
					}
				}
			}
			
			System.out.println(overlapped);
			
		}
		catch (Exception e) {
			System.out.println(e.toString() + " : " + e.getStackTrace().toString());
		}
		return "NULL";
	}
	
	public static void main(String[] args) {
		
		Overlap ovrlp = new Overlap();
		File f = new File("src/day03/input.txt");
		
		ovrlp.parseFile(f);
	}
	
}
