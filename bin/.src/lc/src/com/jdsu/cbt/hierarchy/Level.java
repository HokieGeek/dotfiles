package com.jdsu.cbt.hierarchy;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class Level implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 3262720880377211008L;
	
	private int id; 
	private String name; 
	
	public Level(){}; 
	public Level(int id, String name){
		this.id = id; 
		this.name = name; 
	}
	public int getId() {
		return id;
	}
	public void setId(int id) {
		this.id = id;
	}
	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}
	
	public static final List getAllLevels(){
		List<Level> levelList = new ArrayList<Level>(); 
		
		levelList.add(new Level(0, ""));
		levelList.add(new Level(1, "Region"));
		levelList.add(new Level(2, "State"));
		levelList.add(new Level(3, "Lata"));
		levelList.add(new Level(4, "Community"));
		
		return levelList; 
	}

}
