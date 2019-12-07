package day02;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;

public class Checksum {
	
	Map<Character,Integer> repeats = new HashMap<Character,Integer>();
	
	int twos = 0;
	int threes = 0;
	
	public void parseFile(File file) {
		try 
		{
			Scanner scanner = new Scanner(new BufferedReader(new FileReader(file)));
			
			while(scanner.hasNext()) {
				String s = scanner.next();
				
				for (int i = 0; i < s.length(); i++ ) {
					char c = s.charAt(i);
					if (repeats.containsKey(c)) {
						int numberOfRepeats = repeats.get(c);
						repeats.put(c, ++numberOfRepeats);
					} else {
						repeats.put(c, 1);
					}
				}
				
				boolean twoC = false;
				boolean threeC = false;
				for (int num : repeats.values()) {
					if (num == 2 && !twoC) {
						twos++;
						twoC = true;
					} else if (num == 3 && !threeC) {
						threes++;
						threeC = true;
					}
				}				
				
				repeats.clear();
			}
			
			System.out.println(twos * threes);
			
			scanner.close(); // close stream
		}
		catch (Exception e) {
			System.out.println(e.toString() + " : " + e.getStackTrace().toString());
		}
	}
	
	public static void main(String[] args) {
		
		Checksum chsm = new Checksum();
		File f = new File("src/day02/input.txt");
		
		chsm.parseFile(f);
	}
}
