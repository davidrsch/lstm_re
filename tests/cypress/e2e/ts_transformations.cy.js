describe("Time series transformations card", () => {
  beforeEach(() => {
    cy.visit("/");
    cy.navigate_to_tab("Selecting Features");
    cy.wait(2000);
  });

  it("Transformations dropdown is visible and shows correct options", () => {
    cy.get('[data-testid="selectimeseries"]').should('be.visible').click({ force: true });
    cy.get('[data-testid="selectimeseries-callout"]').should('be.visible');
    cy.get('[data-testid="selectimeseries-callout"]')
      .find('[data-index="0"]').siblings('label')
      .should('have.text', 'Original');
    cy.get('[data-testid="selectimeseries-callout"]')
      .find('[data-index="1"]').siblings('label')
      .should('have.text', 'First transformation');
    cy.get('[data-testid="selectimeseries-callout"]')
      .find('[data-index="2"]').siblings('label')
      .should('have.text', 'Second transformation');
    cy.get('body').type('{esc}');
  });

  it("Scales dropdown is visible and shows correct options", () => {
    cy.get('[data-testid="selectimeseriescales"]').should('be.visible').click({ force: true });
    cy.get('[data-testid="selectimeseriescales-callout"]').should('be.visible');
    cy.get('[data-testid="selectimeseriescales-callout"]')
      .find('[data-index="0"]').siblings('label')
      .should('have.text', 'Exact');
    cy.get('[data-testid="selectimeseriescales-callout"]')
      .find('[data-index="1"]').siblings('label')
      .should('have.text', 'From 0 to 1');
    cy.get('[data-testid="selectimeseriescales-callout"]')
      .find('[data-index="2"]').siblings('label')
      .should('have.text', 'From -1 to 1');
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
