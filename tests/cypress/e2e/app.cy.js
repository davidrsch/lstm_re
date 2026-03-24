describe('App', () => {
  beforeEach(() => {
    cy.visit('/');
  });

  it('starts and renders the main pivot tabs', () => {
    cy.contains('[role="tab"]', 'Welcome').should('be.visible');
    cy.contains('[role="tab"]', 'Upload Data').should('be.visible');
    cy.contains('[role="tab"]', 'Selecting Features').should('be.visible');
    cy.contains('[role="tab"]', 'Results').should('be.visible');
  });

  it('Welcome tab is active by default', () => {
    cy.contains('Powered by').should('be.visible');
  });
});

