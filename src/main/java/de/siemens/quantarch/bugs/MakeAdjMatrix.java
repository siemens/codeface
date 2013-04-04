package de.siemens.quantarch.bugs;

import org.springframework.context.ApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;

import de.siemens.quantarch.bugs.dao.QuantArchBugzillaDAO;

public class MakeAdjMatrix {
	public static void main(String[] args) {
		ApplicationContext context = new ClassPathXmlApplicationContext(
				"beans.xml");
		QuantArchBugzillaDAO bugzillaDAO = (QuantArchBugzillaDAO) context
				.getBean("bugzillaDAO");
		bugzillaDAO.dumpAdjacencyMatrix(null, 1);
	}
}
