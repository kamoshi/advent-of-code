package day02;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.Scanner;

public class Match {
	
	ArrayList<String> container = new ArrayList<>();
	
	public String parseFile(File file) {
		try 
		{
			Scanner scanner = new Scanner(new BufferedReader(new FileReader(file)));
			
			while(scanner.hasNext()) {
				String s = scanner.next();
				
				for (int i=0; i < container.size(); i++) { // Iteration through strings already found
					String checked = container.get(i);
					
					int discrepancy = 0; // differences found between strings
					for (int j = 0; j < s.length(); j++) { // iteration through new string
						if (!(s.charAt(j) == checked.charAt(j))) {
							discrepancy++;
						}
					}
					
					if (discrepancy == 1) {
						System.out.println("FOUND STRINGS MATCH DELTA 1\n" + checked + "\n" + s);
						return s;
					}
				}

				container.add(s);
			}
			scanner.close(); // close stream
		}
		catch (Exception e) {
			System.out.println(e.toString() + " : " + e.getStackTrace().toString());
		}
		return "NULL";
	}
	
	public static void main(String[] args) {
		
		Match chsm = new Match();
		File f = new File("src/day02/input.txt");
		
		chsm.parseFile(f);
	}
}
