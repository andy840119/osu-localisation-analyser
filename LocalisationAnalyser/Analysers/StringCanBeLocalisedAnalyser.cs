// Copyright (c) ppy Pty Ltd <contact@ppy.sh>. Licensed under the MIT Licence.
// See the LICENCE file in the repository root for full licence text.

using System.Collections.Immutable;
using System.ComponentModel;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace LocalisationAnalyser.Analysers
{
    /// <summary>
    /// Discovers all non-verbatim strings (literal and interpolated) and reports <see cref="DiagnosticRules.STRING_CAN_BE_LOCALISED"/>.
    /// </summary>
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class StringCanBeLocalisedAnalyser : DiagnosticAnalyzer
    {
        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(DiagnosticRules.STRING_CAN_BE_LOCALISED);

        public override void Initialize(AnalysisContext context)
        {
            // See https://github.com/dotnet/roslyn/blob/main/docs/analyzers/Analyzer%20Actions%20Semantics.md for more information
            context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.None);
            context.EnableConcurrentExecution();
            context.RegisterSyntaxNodeAction(analyseString, SyntaxKind.StringLiteralExpression, SyntaxKind.InterpolatedStringExpression);
        }

        private void analyseString(SyntaxNodeAnalysisContext context)
        {
            switch (context.Node)
            {
                case LiteralExpressionSyntax literal:
                    if (literal.Token.IsVerbatimStringLiteral())
                        break;

                    // Ignore numeric strings.
                    if (literal.Token.ValueText.All(c => !char.IsLetter(c)))
                        return;

                    // Ignore strings in all attributes other than System.ComponentModel.DescriptionAttribute.
                    if (literal.Parent?.Kind() == SyntaxKind.AttributeArgument)
                    {
                        SyntaxNode attributeSyntax = literal.FirstAncestorOrSelf<AttributeSyntax>();
                        string attributeName = context.SemanticModel.GetSymbolInfo(attributeSyntax).Symbol.ContainingType.ToString();

                        if (attributeName != typeof(DescriptionAttribute).FullName)
                            return;
                    }

                    // Ignore strings if return type is not localisation string.
                    // like: string a = "123";
                    // see: https://stackoverflow.com/a/23878410
                    var declaration = literal.FirstAncestorOrSelf<VariableDeclarationSyntax>();
                    if (declaration != null && declaration.Type.Kind() == SyntaxKind.PredefinedType && declaration.Type.ToString() == "string")
                    {
                        return;
                    }
                    
                    // we do not translate the adding string.
                    if(literal.Parent is BinaryExpressionSyntax)
                        return;
                    
                    // should not convert the 
                    // like: method("aaa");
                    // see: https://stackoverflow.com/a/45362532/4105113
                    var invocation = literal.FirstAncestorOrSelf<InvocationExpressionSyntax>();
                    if (invocation != null)
                    {
                        // get the declaration method
                        var symbolInfo = context
                            .SemanticModel
                            .GetSymbolInfo(invocation, context.CancellationToken);
                        
                        // todo: should not be the first of default.
                        var methodSymbol = symbolInfo.Symbol as IMethodSymbol ??
                                           symbolInfo.CandidateSymbols.FirstOrDefault() as IMethodSymbol;

                        // than, find the index of argument.
                        var argumentSyntax = literal.Parent as ArgumentSyntax;
                        var index = invocation.ArgumentList.Arguments.IndexOf(argumentSyntax);
                        
                        // if argument type is string.
                        if(methodSymbol?.Parameters[index].ToString() == "string")
                            return;
                    }

                    context.ReportDiagnostic(Diagnostic.Create(DiagnosticRules.STRING_CAN_BE_LOCALISED, context.Node.GetLocation(), context.Node));
                    break;

                case InterpolatedStringExpressionSyntax interpolated:
                    if (interpolated.StringStartToken.Kind() == SyntaxKind.InterpolatedVerbatimStringStartToken)
                        break;

                    if (interpolated.Contents.Any(c => c is InterpolatedStringTextSyntax text && text.TextToken.ValueText.Where(char.IsLetter).Any()))
                        context.ReportDiagnostic(Diagnostic.Create(DiagnosticRules.STRING_CAN_BE_LOCALISED, context.Node.GetLocation(), context.Node));
                    break;
            }
        }
    }
}
