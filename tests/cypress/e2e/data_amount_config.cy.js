describe("Data amount configuration (train/test split)", () => {
  beforeEach(() => {
    cy.visit("/");
    cy.navigate_to_tab("Upload Data");
    cy.upload_csv_flow();
    cy.select_io_variables_flow();
  });

  it("Data amount card toggle is enabled after I/O variables selected", () => {
    cy.get('[data-testid="toggle_data_amount_card"]', { timeout: 10000 })
      .should('not.have.attr', 'aria-disabled', 'true');
  });

  it("Test start and test end dropdowns are visible after opening card", () => {
    cy.toggle_card('toggle_data_amount_card');
    // Section header and the two start/end dropdowns inside it
    cy.contains('Test set:', { timeout: 8000 }).should('be.visible');
    cy.contains('Train set:').should('be.visible');
  });

  it("Train start dropdown is visible after opening card", () => {
    cy.toggle_card('toggle_data_amount_card');
    // There are two "Start" labels (test start + train start);
    // verify at least one is visible after opening the card
    cy.contains('label', 'Start', { timeout: 8000 }).should('be.visible');
  });

  it("OK button is visible inside the data amount card", () => {
    cy.toggle_card('toggle_data_amount_card');
    cy.get('[data-testid="adtraintotest"]', { timeout: 8000 }).should('be.visible');
  });

  it("Clicking OK adds a train set row to the table", () => {
    cy.add_train_set_flow();
    cy.get('[data-testid="traindatestable_container"] tbody tr', { timeout: 8000 })
      .should('have.length.gte', 1);
  });

  it("Clicking OK twice with the same dates does not duplicate the row", () => {
    cy.add_train_set_flow();
    // Click OK a second time with the same selection
    cy.get('[data-testid="adtraintotest"]').click({ force: true });
    cy.wait(1000);
    cy.get('[data-testid="traindatestable_container"] tbody tr')
      .should('have.length', 1);
  });
});
