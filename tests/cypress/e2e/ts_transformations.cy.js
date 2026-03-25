describe("Time series transformations card", () => {
  beforeEach(() => {
    cy.visit("/");
    cy.navigate_to_tab("Selecting Features");
    cy.wait(2000);
  });

  it("Transformations dropdown is visible with correct label", () => {
    cy.contains('Time series to use').should('be.visible');
    cy.get('[data-testid="selectimeseries"]').should('be.visible');
  });

  it("Scales dropdown is visible with correct label", () => {
    cy.contains('Scales to use').should('be.visible');
    cy.get('[data-testid="selectimeseriescales"]').should('be.visible');
  });

  it("Transformations dropdown defaults to all three selected", () => {
    cy.get('[data-testid="selectimeseries"]')
      .should('be.visible')
      .should('contain.text', 'Original')
      .should('contain.text', 'First transformation')
      .should('contain.text', 'Second transformation');
  });

  it("Scales dropdown defaults to all three selected", () => {
    cy.get('[data-testid="selectimeseriescales"]')
      .should('be.visible')
      .should('contain.text', 'Exact')
      .should('contain.text', 'From 0 to 1')
      .should('contain.text', 'From -1 to 1');
  });

  it("Transformations dropdown is interactive", () => {
    // Verify the dropdown can be clicked without breaking the UI.
    // (Callout rendering is not tested here as it depends on headless browser behaviour.)
    cy.get('[data-testid="selectimeseries"]').click({ force: true });
    cy.wait(500);
    cy.get('body').type('{esc}');
    cy.get('[data-testid="selectimeseries"]').should('be.visible');
  });
});
