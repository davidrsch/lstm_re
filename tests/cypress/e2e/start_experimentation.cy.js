describe("Start experimentation validation", () => {
  beforeEach(() => {
    cy.visit("/");
    cy.navigate_to_tab("Selecting Features");
    // Wait for the Start button to be visible (confirms page is ready)
    cy.get('[data-testid="startexperimentation"]', { timeout: 10000 }).should('be.visible');
  });

  it("Start button is visible on Selecting Features page", () => {
    cy.get('[data-testid="startexperimentation"]').should('be.visible');
  });

  it("Clicking Start without required inputs shows error modal", () => {
    cy.get('[data-testid="startexperimentation"]').click({ force: true });
    cy.get('[role="dialog"]', { timeout: 10000 }).should('exist');
  });

  it("Error modal can be dismissed", () => {
    cy.get('[data-testid="startexperimentation"]').click({ force: true });
    cy.get('[role="dialog"]', { timeout: 10000 }).should('exist');
    // Find and click the close/OK button inside the modal
    cy.get('[role="dialog"]').find('button').first().click({ force: true });
    cy.wait(500);
    cy.get('[role="dialog"]').should('not.exist');
  });
});
