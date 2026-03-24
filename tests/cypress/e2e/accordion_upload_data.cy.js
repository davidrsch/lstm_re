describe("Upload Data page accordion - mutual exclusion", () => {
  beforeEach(() => {
    cy.visit("/");
    cy.navigate_to_tab("Upload Data");
  });

  it("Opening variables card collapses the upload card", () => {
    // Upload card starts open
    cy.get('[data-testid="file"]').should('be.visible');
    // Open variables card
    cy.toggle_card("toggle_variables_card");
    // Upload card content should now be hidden
    cy.get('[data-testid="file"]').should('not.be.visible');
    // Variables card content should be visible
    cy.get('[data-testid="io_gridtable"]').should('be.visible');
  });

  it("Opening data amount card collapses the upload card", () => {
    // Upload card starts open
    cy.get('[data-testid="file"]').should('be.visible');
    // Open data amount card
    cy.toggle_card("toggle_data_amount_card");
    // Upload card content should now be hidden
    cy.get('[data-testid="file"]').should('not.be.visible');
  });

  it("Re-opening upload card collapses variables card", () => {
    // First open variables card (which collapses upload card)
    cy.toggle_card("toggle_variables_card");
    cy.get('[data-testid="io_gridtable"]').should('be.visible');
    // Now open upload card
    cy.toggle_card("toggle_upload_card");
    // Upload card content should be visible now
    cy.get('[data-testid="file"]').should('be.visible');
    // Variables card content should be hidden
    cy.get('[data-testid="io_gridtable"]').should('not.be.visible');
  });
});
