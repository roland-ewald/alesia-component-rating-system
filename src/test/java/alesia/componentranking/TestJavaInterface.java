/*******************************************************************************
 * Copyright 2012-2013 Jonathan Wienss, Michael Stein, Roland Ewald
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/
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
