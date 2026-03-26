describe("Select variables (I/O grid)", () => {
  beforeEach(() => {
    cy.visit("/");
    cy.navigate_to_tab("Upload Data");
    cy.upload_csv_flow();
  });

  it("Variables card toggle button becomes enabled after upload", () => {
    cy.get('[data-testid="toggle_variables_card"]', { timeout: 10000 })
      .should('not.have.attr', 'aria-disabled', 'true');
  });

  it("IO grid is visible after opening variables card", () => {
    cy.toggle_card('toggle_variables_card');
    cy.get('[data-testid="io_gridtable"]', { timeout: 10000 }).should('be.visible');
  });

  it("IO grid has Inputs, Outputs and Variables columns", () => {
    cy.toggle_card('toggle_variables_card');
    cy.get('[data-testid="io_gridtable"] thead th', { timeout: 10000 })
      .should('contain.text', 'Inputs')
      .and('contain.text', 'Outputs')
      .and('contain.text', 'Variables');
  });

  it("IO grid shows data columns from the uploaded CSV", () => {
    cy.toggle_card('toggle_variables_card');
    // csv_example.csv has columns: date, col1, col2
    // The date column is excluded from the grid (used as date variable)
    // The grid should contain the non-date columns: col1, col2
    cy.get('[data-testid="io_gridtable"] tbody', { timeout: 10000 })
      .should('contain.text', 'col1')
      .and('contain.text', 'col2');
  });

  it("Data amount card toggle is disabled before I/O variables are selected", () => {
    cy.get('[data-testid="toggle_data_amount_card"]', { timeout: 10000 })
      .should('have.attr', 'aria-disabled', 'true');
  });

  it("Selecting all inputs and outputs enables the data amount card toggle", () => {
    cy.select_io_variables_flow();
    cy.get('[data-testid="toggle_data_amount_card"]', { timeout: 10000 })
      .should('not.have.attr', 'aria-disabled', 'true');
  });
});
