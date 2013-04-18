package alesia.componentranking;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import alesia.componentrating.ComponentRatingSystem;
import alesia.componentrating.IComponentRatingSystem;
import alesia.componentrating.TrueSkillRatingSystem;

/**
 * Some simple tests for the Java interface, {@link IComponentRatingSystem}.
 * 
 * @see ComponentRatingSystem
 * @see TrueSkillRatingSystem
 * 
 * @author Roland Ewald
 */
public class TestJavaInterface {

	// Player/component IDs:

	private static final String DIONYSOS = "Dionysos";

	private static final String CAESAR = "Caesar";

	private static final String BOB = "Bob";

	private static final String ALAN = "Alan";

	/**
	 * The component rating system.
	 */
	private IComponentRatingSystem crs;

	@Before
	public void setUp() {
		crs = TrueSkillRatingSystem.createDefaultSetup();
	}

	@Test
	public void compareTwoUnknownComponents() {
		Assert.assertEquals("Unknown components should be weighted equally", 0,
				crs.compare("A", "B"));
	}

	@Test
	public void simpleComparison() {

		// Create component team results
		List<Set<String>> results = new ArrayList<Set<String>>();
		Set<String> teamA = new HashSet<String>();
		teamA.add(CAESAR);
		teamA.add(DIONYSOS);
		Set<String> teamB = new HashSet<String>();
		teamB.add(ALAN);
		teamB.add(BOB);
		results.add(teamA);
		results.add(teamB);

		crs.submitResults(results);

		// Check of comparisons yield the expected results
		Assert.assertTrue(crs.compare(ALAN, CAESAR) < 0);
		Assert.assertTrue(crs.compare(DIONYSOS, BOB) > 0);
	}
}
