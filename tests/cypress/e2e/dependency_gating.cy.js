describe("Dependency gating - disabled toggle buttons", () => {
  beforeEach(() => {
    cy.visit("/");
    cy.navigate_to_tab("Upload Data");
  });

  it("Variables card toggle is disabled before data is uploaded", () => {
    cy.get('[data-testid="toggle_variables_card"]').should('be.disabled');
  });

  it("Variables card toggle becomes enabled after uploading data", () => {
    cy.upload_csv_flow();
    cy.get('[data-testid="toggle_variables_card"]').should('not.be.disabled');
  });

  it("Data amount card toggle is disabled before data is uploaded", () => {
    cy.get('[data-testid="toggle_data_amount_card"]').should('be.disabled');
  });

  it("Data amount card toggle remains disabled after upload without I/O variables selected", () => {
    cy.upload_csv_flow();
    // Toggle is still disabled because no input/output variables have been selected
    cy.get('[data-testid="toggle_data_amount_card"]').should('be.disabled');
  });
});
