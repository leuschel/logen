<?xml version="1.0" ?>
<!DOCTYPE xsl:stylesheet 
[ 
<!ENTITY nbsp "&#160;"> <!-- white space in XSL -->
]>   
<xsl:stylesheet version = '1.0'
	xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>

	<xsl:template match="/">
		<html>
		<head>
			<title>
				<xsl:text>Annotated Prolog file</xsl:text>
			</title>
			<link type="text/css" rel="stylesheet" href="prolog.css"/>
		</head>
		<body>
			<xsl:apply-templates/>
		</body>
		</html>
	</xsl:template>

	<xsl:template match="source">
		<h2>Annotated Source Code</h2>
		<p>
		<xsl:apply-templates/>
		</p>
	</xsl:template>

	<xsl:template match="filters">
		<h2>Filters</h2>
		<p>
		<xsl:apply-templates/>
		</p>
	</xsl:template>
	<xsl:template match="text()|@*">
	    <xsl:call-template name="general-spaces-replace">
			<xsl:with-param name="word" select="."/>
		</xsl:call-template>
	</xsl:template>

	<xsl:template match="hide_nf">
		<xsl:element name="span">
			<xsl:attribute name="class">
				<xsl:value-of select="name()"/>
			</xsl:attribute>
			<!-- <xsl:apply-templates select="*"/> -->
			<xsl:apply-templates select="*">
				<xsl:with-param name="enclosing">
					<xsl:value-of select="name()"/>
					<xsl:text>, </xsl:text>
				</xsl:with-param>
			</xsl:apply-templates>
		</xsl:element>
	</xsl:template> 

	<xsl:template match="unfold|memo|dynamic|static|call|rescall|unsafe|ucall|if|logif|semicall|mcall|resif|semif|reslogif|unknown|findall|resfindall|resnot|not|disj|resdisj|mnf|pp_cll|pp_mnf|when|semiwhen|reswhen|online|time_out">
		<xsl:param name="enclosing" select="''"/>
		<xsl:element name="span">
			<xsl:attribute name="class">
				<xsl:value-of select="name()"/>
			</xsl:attribute>
			<xsl:value-of select="."/>
			<xsl:element name="div">
				<xsl:value-of select="concat($enclosing, name())"/>
			</xsl:element>
		</xsl:element>
	</xsl:template> 

	<xsl:template match="head|comment|filter">
		<xsl:element name="span">
			<xsl:attribute name="class">
				<xsl:value-of select="name()"/>
			</xsl:attribute>
			<xsl:call-template name="general-spaces-replace">
				<xsl:with-param name="word" select="."/>
			</xsl:call-template>
		</xsl:element>
	</xsl:template> 

	<xsl:template name="general-spaces-replace" >
		<xsl:param name="word"/>
		<xsl:param name="i">0</xsl:param>
		<xsl:if test="$i&lt;string-length(.) ">
		    <!-- New line -->
			<xsl:if test="substring($word,1,1)='&#10;'">
            	<br/>
				<xsl:text>
</xsl:text>
				<xsl:call-template name="general-spaces-replace">
					<xsl:with-param name="word" select="substring($word,2)"/>
					<xsl:with-param name="i" select="$i + 1"/>
				</xsl:call-template>
			</xsl:if>
			<xsl:if test="substring($word,1,1)='&#32;'">  <!-- An ordinary space -->
				<xsl:text disable-output-escaping="yes">&amp;nbsp;</xsl:text>
				<xsl:call-template name="general-spaces-replace">
					<xsl:with-param name="word" select="substring($word,2)"/>
					<xsl:with-param name="i" select="$i + 1"/>
				</xsl:call-template>
			</xsl:if>
			<xsl:if test="substring($word,1,1)='&#9;'">  <!-- A tab is replaced with 4 spaces  -->
				<xsl:text disable-output-escaping="yes">&amp;nbsp;&amp;nbsp;&amp;nbsp;&amp;nbsp;</xsl:text>
				<xsl:call-template name="general-spaces-replace">
					<xsl:with-param name="word" select="substring($word,2)"/>
					<xsl:with-param name="i" select="$i + 1"/>
				</xsl:call-template>
			</xsl:if>
			<xsl:if test="not(substring($word,1,1)='&#9;' or substring($word,1,1)='&#32;' or substring($word,1,1)='&#10;')">
				<xsl:value-of select = "substring($word,1,1)"/>
				<xsl:call-template name="general-spaces-replace">
					<xsl:with-param name="word" select="substring($word,2)"/>
					<xsl:with-param name="i" select="$i + 1"/>
				</xsl:call-template>
			</xsl:if>
		</xsl:if>
	</xsl:template>
</xsl:stylesheet> 
