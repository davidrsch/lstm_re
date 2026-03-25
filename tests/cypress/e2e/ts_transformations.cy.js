describe("Time series transformations card", () => {
  beforeEach(() => {
    cy.visit("/");
    cy.navigate_to_tab("Selecting Features");
    cy.wait(2000);
  });

  it("Transformations dropdown is visible and shows correct options", () => {
    cy.get('[data-testid="selectimeseries"]').should('be.visible').click({ force: true });
    cy.contains('[role="option"]', 'Original').should('be.visible');
    cy.contains('[role="option"]', 'First transformation').should('be.visible');
    cy.contains('[role="option"]', 'Second transformation').should('be.visible');
    cy.get('body').type('{esc}');
  });

  it("Scales dropdown is visible and shows correct options", () => {
    cy.get('[data-testid="selectimeseriescales"]').should('be.visible').click({ force: true });
    cy.contains('[role="option"]', 'Exact').should('be.visible');
    cy.contains('[role="option"]', 'From 0 to 1').should('be.visible');
    cy.contains('[role="option"]', 'From -1 to 1').should('be.visible');
    cy.get('body').type('{esc}');
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

  it("Can select a single transformation", () => {
    cy.select_dropdown('selectimeseries', [0]);
    cy.get('[data-testid="selectimeseries"]')
      .should('have.text', 'Original');
  });
});
