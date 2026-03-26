describe("Upload card accordion", () => {
  beforeEach(() => {
    cy.visit("/");
    cy.navigate_to_tab("Upload Data");
  });

  it("Upload card starts expanded (content visible)", () => {
    cy.get('[data-testid="file"]').should('be.visible');
  });

  it("Clicking toggle collapses the upload card", () => {
    cy.toggle_card("toggle_upload_card");
    cy.get('[data-testid="file"]', { timeout: 8000 }).should('not.be.visible');
  });

  it("Clicking toggle again expands the upload card", () => {
    cy.toggle_card("toggle_upload_card");
    cy.toggle_card("toggle_upload_card");
    cy.get('[data-testid="file"]', { timeout: 8000 }).should('be.visible');
  });

  it("Variables card starts collapsed", () => {
    cy.get('[data-testid="toggle_variables_card"]').should('be.visible');
    // The variables card content (io_gridtable) should not be visible initially
    cy.get('[data-testid="io_gridtable"]').should('not.be.visible');
  });

  it("Clicking variables card toggle expands it", () => {
    cy.upload_csv_flow();
    cy.toggle_card("toggle_variables_card");
    cy.get('[data-testid="io_gridtable"]', { timeout: 8000 }).should('be.visible');
  });
});
