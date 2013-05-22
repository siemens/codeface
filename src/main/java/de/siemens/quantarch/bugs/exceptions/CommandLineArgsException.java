package de.siemens.quantarch.bugs.exceptions;

public class CommandLineArgsException extends Exception {

	private static final long serialVersionUID = -5797860031871610760L;

	public CommandLineArgsException(String msg) {
		super(msg);
	}

	public CommandLineArgsException() {
		super();
	}
}
