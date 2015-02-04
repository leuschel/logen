<?xml version="1.0" ?>
<!DOCTYPE xsl:stylesheet 
[ 
<!ENTITY nbsp "&#160;"> <!-- white space in XSL -->
]>   
<xsl:stylesheet 
  version = '1.0'
  xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>
	<xsl:output method="text"/>
	

	<xsl:template match="/">
<xsl:text>\documentclass{article}
\usepackage{latexsym}
\usepackage{amsfonts}
\usepackage{amssymb}
\usepackage{listings}
\lstset{captionpos=b,
	xleftmargin=18pt,
	basicstyle=\small \ttfamily,
	showstringspaces=false,
	tabsize=4,
	keepspaces=true,
	mathescape=true}

\begin{document}
</xsl:text>
		<xsl:apply-templates/>
<xsl:text>
\end{document}
</xsl:text>
	</xsl:template>

	<xsl:template match="source">
<xsl:text>\section{Annotated Source Code}

\begin{lstlisting}
</xsl:text>

		<xsl:apply-templates/>
<xsl:text></xsl:text>
	</xsl:template>

	<xsl:template match="filters">
<xsl:text>
\end{lstlisting}

\section{Filters}

\begin{lstlisting}
</xsl:text>
		<xsl:apply-templates/>
<xsl:text>
\end{lstlisting}
</xsl:text>
	</xsl:template>

	<xsl:template match="text()|@*">
	    <xsl:call-template name="replace-dollars">
			<xsl:with-param name="word" select="."/>
		</xsl:call-template>
	</xsl:template>

	<xsl:template match="hide_nf">
		<xsl:text>$\mathit{hide\_nf}($</xsl:text>
		<xsl:apply-templates select="*">
		</xsl:apply-templates>
		<xsl:text>$)$</xsl:text>
	</xsl:template> 

	<xsl:template match="unfold|memo|call|rescall|unsafe|ucall|if|logif|semicall|mcall|resif|semif|reslogif|unknown|findall|resfindall|resnot|not|disj|resdisj|mnf|pp_cll|pp_mnf|when|semiwhen|reswhen|online|time_out">
		<xsl:text>$\underbrace{\mathtt{</xsl:text>
	    <xsl:call-template name="general-spaces-replace">
			<xsl:with-param name="word" select="."/>
		</xsl:call-template>
		<xsl:text>}}_\mathit{</xsl:text>
		<xsl:value-of select="name()"/>
		<xsl:text>}$</xsl:text>
	</xsl:template> 

	<xsl:template match="head|comment|filter|static|dynamic">
		<xsl:value-of select="."/>
	</xsl:template> 

	<xsl:template name="replace-dollars">
		<xsl:param name="word"/>
		<xsl:param name="i">0</xsl:param>

		
		<xsl:if test="$i&lt;string-length(.) ">
			<xsl:choose>
				<xsl:when test="substring($word,1,1)='$'">
					<xsl:text>\$</xsl:text>
					<xsl:call-template name="replace-dollars">
						<xsl:with-param name="word" select="substring($word,2)"/>
						<xsl:with-param name="i" select="$i + 1"/>
					</xsl:call-template>
				</xsl:when>
				<xsl:otherwise>
					<xsl:value-of select = "substring($word,1,1)"/>
					<xsl:call-template name="replace-dollars">
						<xsl:with-param name="word" select="substring($word,2)"/>
						<xsl:with-param name="i" select="$i + 1"/>
					</xsl:call-template>
				</xsl:otherwise>
			</xsl:choose>
		</xsl:if>
	</xsl:template>

	<xsl:template name="general-spaces-replace" >
		<xsl:param name="word"/>
		<xsl:param name="i">0</xsl:param>

		
		<xsl:if test="$i&lt;string-length(.) ">
			<xsl:choose>
				<xsl:when test="substring($word,1,1)='$'">
					<xsl:text>\mbox{\$}</xsl:text>
					<xsl:call-template name="replace-dollars">
						<xsl:with-param name="word" select="substring($word,2)"/>
						<xsl:with-param name="i" select="$i + 1"/>
					</xsl:call-template>
				</xsl:when>
				<xsl:when test="substring($word,1,1)='_'">
					<xsl:text>\_</xsl:text>
					<xsl:call-template name="general-spaces-replace">
						<xsl:with-param name="word" select="substring($word,2)"/>
						<xsl:with-param name="i" select="$i + 1"/>
					</xsl:call-template>
				</xsl:when>
				<xsl:when test="substring($word,1,1)='\'">
					<xsl:text>\backslash</xsl:text>
					<xsl:call-template name="general-spaces-replace">
						<xsl:with-param name="word" select="substring($word,2)"/>
						<xsl:with-param name="i" select="$i + 1"/>
					</xsl:call-template>
				</xsl:when>
				<xsl:when test="substring($word,1,1)='&#10;'">
					<xsl:text>\\</xsl:text>
					<xsl:call-template name="general-spaces-replace">
						<xsl:with-param name="word" select="substring($word,2)"/>
						<xsl:with-param name="i" select="$i + 1"/>
					</xsl:call-template>
				</xsl:when>
				<xsl:when test="substring($word,1,1)='&#32;'">  <!-- An ordinary space -->
					<xsl:text disable-output-escaping="yes">\ </xsl:text>
					<xsl:call-template name="general-spaces-replace">
						<xsl:with-param name="word" select="substring($word,2)"/>
						<xsl:with-param name="i" select="$i + 1"/>
					</xsl:call-template>
				</xsl:when>
				<xsl:when test="substring($word,1,1)='&#9;'">  <!-- A tab is replaced with 4 spaces  -->
					<xsl:text disable-output-escaping="yes">\ \ \ \ </xsl:text>
					<xsl:call-template name="general-spaces-replace">
						<xsl:with-param name="word" select="substring($word,2)"/>
						<xsl:with-param name="i" select="$i + 1"/>
					</xsl:call-template>
				</xsl:when>
				<xsl:when test="substring($word,1,1)='='">
					<xsl:text disable-output-escaping="yes">\mbox{=}</xsl:text>
					<xsl:call-template name="general-spaces-replace">
						<xsl:with-param name="word" select="substring($word,2)"/>
						<xsl:with-param name="i" select="$i + 1"/>
					</xsl:call-template>
				</xsl:when>
				<xsl:when test="substring($word,1,1)='-'">
					<xsl:text disable-output-escaping="yes">\mbox{-}</xsl:text>
					<xsl:call-template name="general-spaces-replace">
						<xsl:with-param name="word" select="substring($word,2)"/>
						<xsl:with-param name="i" select="$i + 1"/>
					</xsl:call-template>
				</xsl:when>
				<xsl:when test="substring($word,1,1)='&lt;'">
					<xsl:text disable-output-escaping="yes">\mbox{&lt;}</xsl:text>
					<xsl:call-template name="general-spaces-replace">
						<xsl:with-param name="word" select="substring($word,2)"/>
						<xsl:with-param name="i" select="$i + 1"/>
					</xsl:call-template>
				</xsl:when>
				<xsl:when test="substring($word,1,1)='&gt;'">
					<xsl:text disable-output-escaping="yes">\mbox{&gt;}</xsl:text>
					<xsl:call-template name="general-spaces-replace">
						<xsl:with-param name="word" select="substring($word,2)"/>
						<xsl:with-param name="i" select="$i + 1"/>
					</xsl:call-template>
				</xsl:when>
				<xsl:otherwise>
					<xsl:value-of select = "substring($word,1,1)"/>
					<xsl:call-template name="general-spaces-replace">
						<xsl:with-param name="word" select="substring($word,2)"/>
						<xsl:with-param name="i" select="$i + 1"/>
					</xsl:call-template>
				</xsl:otherwise>
			</xsl:choose>
		</xsl:if>
	</xsl:template>
</xsl:stylesheet> 
