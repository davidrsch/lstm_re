describe("Upload Data page accordion - mutual exclusion", () => {
  beforeEach(() => {
    cy.visit("/");
    cy.navigate_to_tab("Upload Data");
  });

  it("Opening variables card collapses the upload card", () => {
    // Upload a file first to enable the variables card toggle
    cy.upload_csv_flow();
    // Upload card starts open
    cy.get('[data-testid="file"]').should('be.visible');
    // Open variables card
    cy.toggle_card("toggle_variables_card");
    // Upload card content should now be hidden
    cy.get('[data-testid="file"]', { timeout: 8000 }).should('not.be.visible');
    // Variables card content should be visible
    cy.get('[data-testid="io_gridtable"]', { timeout: 8000 }).should('be.visible');
  });

  it("Data amount card toggle is disabled until input/output variables are selected", () => {
    // Without upload, data amount card should be disabled
    cy.get('[data-testid="toggle_data_amount_card"]', { timeout: 10000 })
      .then(($el) => {
        cy.log('toggle_data_amount_card (accordion): disabled=' + $el.attr('disabled') +
          ' aria-disabled=' + $el.attr('aria-disabled') +
          ' tag=' + $el.prop('tagName') +
          ' class=' + ($el.attr('class') || '').substring(0, 60));
        return $el;
      })
      .should('have.attr', 'aria-disabled', 'true');
    // After upload it is still disabled (no I/O variables selected yet)
    cy.upload_csv_flow();
    cy.get('[data-testid="toggle_data_amount_card"]', { timeout: 8000 }).should('have.attr', 'aria-disabled', 'true');
  });

  it("Re-opening upload card collapses variables card", () => {
    // Upload a file first to enable the variables card toggle
    cy.upload_csv_flow();
    // First open variables card (which collapses upload card)
    cy.toggle_card("toggle_variables_card");
    cy.get('[data-testid="io_gridtable"]').should('be.visible');
    // Now open upload card
    cy.toggle_card("toggle_upload_card");
    // Upload card content should be visible now
    cy.get('[data-testid="file"]', { timeout: 8000 }).should('be.visible');
    // Variables card content should be hidden
    cy.get('[data-testid="io_gridtable"]', { timeout: 8000 }).should('not.be.visible');
  });
});
