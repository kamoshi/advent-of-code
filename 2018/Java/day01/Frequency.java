package day01;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.HashSet;
import java.util.Scanner;

public class Frequency {
	
	private HashSet<Integer> pastValues = new HashSet<Integer>();
	
	private int frequency = 0;
	private boolean repeated;
	
	public void setRepeated() {
		this.repeated = true;
	}
	
	public boolean getRepeated() {
		return this.repeated;
	}
	
	public void plusFrequency(int plus) {
		this.frequency += plus;
	}
	
	public void minusFrequency(int minus) {
		this.frequency -= minus;
	}
	
	public int getFrequency() {
		return this.frequency;
	}
	
	public boolean compareHashSet(int number) {
		return !pastValues.add(number);
	}
	
	public void parseFile(File file) {
		try {
			Scanner scanner = new Scanner(new BufferedReader(new FileReader(file)));
			
			while(scanner.hasNext()) {
				String s = scanner.next();
				if (s.matches("(\\-)(\\d)+")) {
					this.minusFrequency(Integer.parseInt(s.substring(1)));
				} else {
					this.plusFrequency(Integer.parseInt(s.substring(1)));
				}
				if (compareHashSet(this.getFrequency())) {
					System.out.println("repeat: " + this.getFrequency());
					this.setRepeated();
					break;
				}
			}
			
			scanner.close();
		}
		catch (Exception e) {
			System.out.println(e.toString() + " : " + e.getStackTrace().toString());
		}
	}
	
	public static void main(String[] args) {
		
		File input = new File("src/day01/input.txt");

		Frequency f = new Frequency();
		
		while ( !f.getRepeated() ) {
			f.parseFile(input);
			System.out.println(f.getFrequency());
		}
		
	}
}
