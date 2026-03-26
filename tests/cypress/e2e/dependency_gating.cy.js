describe("Dependency gating - disabled toggle buttons", () => {
  beforeEach(() => {
    cy.visit("/");
    cy.navigate_to_tab("Upload Data");
  });

  it("Variables card toggle is disabled before data is uploaded", () => {
    cy.get('[data-testid="toggle_variables_card"]', { timeout: 10000 })
      .then(($el) => {
        cy.log('toggle_variables_card: disabled=' + $el.attr('disabled') +
          ' aria-disabled=' + $el.attr('aria-disabled') +
          ' tag=' + $el.prop('tagName') +
          ' class=' + ($el.attr('class') || '').substring(0, 60));
      })
      .should('have.attr', 'aria-disabled', 'true');
  });

  it("Variables card toggle becomes enabled after uploading data", () => {
    cy.upload_csv_flow();
    cy.get('[data-testid="toggle_variables_card"]', { timeout: 10000 }).should('not.be.disabled');
  });

  it("Data amount card toggle is disabled before data is uploaded", () => {
    cy.get('[data-testid="toggle_data_amount_card"]', { timeout: 10000 })
      .then(($el) => {
        cy.log('toggle_data_amount_card: disabled=' + $el.attr('disabled') +
          ' aria-disabled=' + $el.attr('aria-disabled') +
          ' tag=' + $el.prop('tagName') +
          ' class=' + ($el.attr('class') || '').substring(0, 60));
      })
      .should('have.attr', 'aria-disabled', 'true');
  });

  it("Data amount card toggle remains disabled after upload without I/O variables selected", () => {
    cy.upload_csv_flow();
    // Toggle is still disabled because no input/output variables have been selected
    cy.get('[data-testid="toggle_data_amount_card"]', { timeout: 10000 })
      .then(($el) => {
        cy.log('toggle_data_amount_card after upload: disabled=' + $el.attr('disabled') +
          ' aria-disabled=' + $el.attr('aria-disabled') +
          ' tag=' + $el.prop('tagName') +
          ' class=' + ($el.attr('class') || '').substring(0, 60));
      })
      .should('have.attr', 'aria-disabled', 'true');
  });
});
