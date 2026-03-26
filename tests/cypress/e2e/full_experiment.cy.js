// Full end-to-end experiment: upload → select variables → configure data amount
// → configure experiment parameters → run → wait for training to complete →
// verify download becomes available.
//
// This test specifically catches the "disconnect during training" regression
// by actually executing a minimal LSTM training run and asserting that the
// server survives until experiment_done(TRUE) is set.
//
// Training is intentionally minimal (1 epoch, 1 LSTM layer, 4 neurons) so the
// run completes in ≈ 30–90 seconds on CI with the 20-row csv_example.csv.

describe("Full experiment end-to-end", () => {
  // Long timeout: training + rmarkdown render can take up to 5 minutes on CI.
  const TRAINING_TIMEOUT = 5 * 60 * 1000;

  before(() => {
    cy.visit("/");
    cy.navigate_to_tab("Upload Data");

    // Step 1: Upload data
    cy.upload_csv_flow();

    // Step 2: Select I/O variables
    cy.select_io_variables_flow();

    // Step 3: Configure train/test data split
    cy.add_train_set_flow();

    // Step 4: Navigate to Selecting Features and configure experiment
    cy.navigate_to_tab("Selecting Features");
    cy.configure_experiment_flow();

    // Step 5: Start the experiment
    cy.start_experiment_flow();
  });

  it("navigates to the Results tab automatically after accepting the experiment", () => {
    cy.tab_should_be_active("Results");
  });

  it("shows a progress panel in the Results tab during/after training", () => {
    // The results_display module creates a PivotItem with a training_progress
    // wellPanel that contains the live plotting area.
    cy.get('.ms-PivotItem', { timeout: 15000 }).should('exist');
  });

  it("Download button becomes enabled after training completes", () => {
    // This assertion will block until training finishes or the timeout expires.
    // A Shiny disconnect would cause the element to never lose aria-disabled,
    // failing this test and surfacing the bug.
    cy.contains('[role="menuitem"]', 'Download', { timeout: TRAINING_TIMEOUT })
      .should('not.have.attr', 'aria-disabled', 'true');
  });

  it("Download submenu contains Dashboard and Models options", () => {
    // Hover over the now-enabled Download menu to reveal the submenu
    cy.contains('[role="menuitem"]', 'Download').click({ force: true });
    cy.contains('[role="menuitem"]', 'Dashboard', { timeout: 5000 }).should('be.visible');
    cy.contains('[role="menuitem"]', 'Models').should('be.visible');
    // Close the submenu
    cy.get('body').type('{esc}');
  });
});
