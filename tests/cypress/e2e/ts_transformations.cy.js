describe("Time series transformations card", () => {
  beforeEach(() => {
    cy.visit("/");
    cy.navigate_to_tab("Selecting Features");
    cy.wait(2000);
  });

  it("Transformations dropdown is visible and shows correct options", () => {
    cy.get('[data-testid="selectimeseries"]').should('be.visible').click({ force: true });
    cy.get('[role="listbox"]').should('exist');
    cy.get('[role="listbox"]').contains('[role="option"]', 'Original').should('exist');
    cy.get('[role="listbox"]').contains('[role="option"]', 'First transformation').should('exist');
    cy.get('[role="listbox"]').contains('[role="option"]', 'Second transformation').should('exist');
    cy.get('body').type('{esc}');
  });

  it("Scales dropdown is visible and shows correct options", () => {
    cy.get('[data-testid="selectimeseriescales"]').should('be.visible').click({ force: true });
    cy.get('[role="listbox"]').should('exist');
    cy.get('[role="listbox"]').contains('[role="option"]', 'Exact').should('exist');
    cy.get('[role="listbox"]').contains('[role="option"]', 'From 0 to 1').should('exist');
    cy.get('[role="listbox"]').contains('[role="option"]', 'From -1 to 1').should('exist');
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
    // All 3 options are selected by default. Deselect indices 1 & 2
    // to leave only index 0 ("Original") selected.
    cy.select_dropdown('selectimeseries', [1, 2]);
    cy.get('[data-testid="selectimeseries"]')
      .should('contain.text', 'Original');
  });
});
