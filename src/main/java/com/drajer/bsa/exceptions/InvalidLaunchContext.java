package com.drajer.bsa.exceptions;

public class InvalidLaunchContext extends Exception {
	
	public InvalidLaunchContext(String message) {
		super(message);
	}
	
	public InvalidLaunchContext(String message, Throwable throwable) {
		super(message,throwable);
	}

}
