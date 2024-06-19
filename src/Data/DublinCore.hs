{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : DublinCore
Description : Dublin core metadata terms
Copyright   : (c) Doug Palmer
2024
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

URIs for dublin core metadata
-}
module Data.DublinCore (module Data.DublinCore) where


import Data.Maybe  
import Network.URI

-- Dublin core elements

dcContributor :: URI
dcContributor = fromJust $ parseURI "http://purl.org/dc/elements/1.1/contributor"
dcCoverage :: URI
dcCoverage = fromJust $ parseURI "http://purl.org/dc/elements/1.1/coverage"
dcCreator :: URI
dcCreator = fromJust $ parseURI "http://purl.org/dc/elements/1.1/creator"
dDdate :: URI
dDdate = fromJust $ parseURI "http://purl.org/dc/elements/1.1/date"
dcDescription :: URI
dcDescription = fromJust $ parseURI "http://purl.org/dc/elements/1.1/description"
dcFormat :: URI
dcFormat = fromJust $ parseURI "http://purl.org/dc/elements/1.1/format"
dcIdentifier :: URI
dcIdentifier = fromJust $ parseURI "http://purl.org/dc/elements/1.1/identifier"
dcLanguage :: URI
dcLanguage = fromJust $ parseURI "http://purl.org/dc/elements/1.1/language"
dcPublisher :: URI
dcPublisher = fromJust $ parseURI "http://purl.org/dc/elements/1.1/publisher"
dcRelation :: URI
dcRelation = fromJust $ parseURI "http://purl.org/dc/elements/1.1/relation"
dcRights :: URI
dcRights = fromJust $ parseURI "http://purl.org/dc/elements/1.1/rights"
dcSource :: URI
dcSource = fromJust $ parseURI "http://purl.org/dc/elements/1.1/source"
dcSubject :: URI
dcSubject = fromJust $ parseURI "http://purl.org/dc/elements/1.1/subject"
dcTitle :: URI
dcTitle = fromJust $ parseURI "http://purl.org/dc/elements/1.1/title"
dcType :: URI
dcType = fromJust $ parseURI "http://purl.org/dc/elements/1.1/type"

-- Dublin core terms

dctermsAbstract :: URI
dctermsAbstract = fromJust $ parseURI "http://purl.org/dc/terms/abstract"
dctermsAccessRights :: URI
dctermsAccessRights = fromJust $ parseURI "http://purl.org/dc/terms/accessRights"
dctermsAccrualMethod :: URI
dctermsAccrualMethod = fromJust $ parseURI "http://purl.org/dc/terms/accrualMethod"
dctermsAccrualPeriodicity :: URI
dctermsAccrualPeriodicity = fromJust $ parseURI "http://purl.org/dc/terms/accrualPeriodicity"
dctermsAccrualPolicy :: URI
dctermsAccrualPolicy = fromJust $ parseURI "http://purl.org/dc/terms/accrualPolicy"
dctermsAlternative :: URI
dctermsAlternative = fromJust $ parseURI "http://purl.org/dc/terms/alternative"
dctermsAudience :: URI
dctermsAudience = fromJust $ parseURI "http://purl.org/dc/terms/audience"
dctermsAvailable :: URI
dctermsAvailable = fromJust $ parseURI "http://purl.org/dc/terms/available"
dctermsBibliographicCitation :: URI
dctermsBibliographicCitation = fromJust $ parseURI "http://purl.org/dc/terms/bibliographicCitation"
dctermsConformsTo :: URI
dctermsConformsTo = fromJust $ parseURI "http://purl.org/dc/terms/conformsTo"
dctermsContributor :: URI
dctermsContributor = fromJust $ parseURI "http://purl.org/dc/terms/contributor"
dctermsCoverage :: URI
dctermsCoverage = fromJust $ parseURI "http://purl.org/dc/terms/coverage"
dctermsCreated :: URI
dctermsCreated = fromJust $ parseURI "http://purl.org/dc/terms/created"
dctermsCreator :: URI
dctermsCreator = fromJust $ parseURI "http://purl.org/dc/terms/creator"
dctermsDate :: URI
dctermsDate = fromJust $ parseURI "http://purl.org/dc/terms/date"
dctermsDateAccepted :: URI
dctermsDateAccepted = fromJust $ parseURI "http://purl.org/dc/terms/dateAccepted"
dctermsDateCopyrighted :: URI
dctermsDateCopyrighted = fromJust $ parseURI "http://purl.org/dc/terms/dateCopyrighted"
dctermsDateSubmitted :: URI
dctermsDateSubmitted = fromJust $ parseURI "http://purl.org/dc/terms/dateSubmitted"
dctermsDescription :: URI
dctermsDescription = fromJust $ parseURI "http://purl.org/dc/terms/description"
dctermsEducationLevel :: URI
dctermsEducationLevel = fromJust $ parseURI "http://purl.org/dc/terms/educationLevel"
dctermsExtent :: URI
dctermsExtent = fromJust $ parseURI "http://purl.org/dc/terms/extent"
dctermsFormat :: URI
dctermsFormat = fromJust $ parseURI "http://purl.org/dc/terms/format"
dctermsHasFormat :: URI
dctermsHasFormat = fromJust $ parseURI "http://purl.org/dc/terms/hasFormat"
dctermsHasPart :: URI
dctermsHasPart = fromJust $ parseURI "http://purl.org/dc/terms/hasPart"
dctermsHasVersion :: URI
dctermsHasVersion = fromJust $ parseURI "http://purl.org/dc/terms/hasVersion"
dctermsIdentifier :: URI
dctermsIdentifier = fromJust $ parseURI "http://purl.org/dc/terms/identifier"
dctermsInstructionalMethod :: URI
dctermsInstructionalMethod = fromJust $ parseURI "http://purl.org/dc/terms/instructionalMethod"
dctermsIsFormatOf :: URI
dctermsIsFormatOf = fromJust $ parseURI "http://purl.org/dc/terms/isFormatOf"
dctermsIsPartOf :: URI
dctermsIsPartOf = fromJust $ parseURI "http://purl.org/dc/terms/isPartOf"
dctermsIsReferencedBy :: URI
dctermsIsReferencedBy = fromJust $ parseURI "http://purl.org/dc/terms/isReferencedBy"
dctermsIsReplacedBy :: URI
dctermsIsReplacedBy = fromJust $ parseURI "http://purl.org/dc/terms/isReplacedBy"
dctermsIsRequiredBy :: URI
dctermsIsRequiredBy = fromJust $ parseURI "http://purl.org/dc/terms/isRequiredBy"
dctermsIssued :: URI
dctermsIssued = fromJust $ parseURI "http://purl.org/dc/terms/issued"
dctermsIsVersionOf :: URI
dctermsIsVersionOf = fromJust $ parseURI "http://purl.org/dc/terms/isVersionOf"
dctermsLanguage :: URI
dctermsLanguage = fromJust $ parseURI "http://purl.org/dc/terms/language"
dctermsLicense :: URI
dctermsLicense = fromJust $ parseURI "http://purl.org/dc/terms/license"
dctermsMediator :: URI
dctermsMediator = fromJust $ parseURI "http://purl.org/dc/terms/mediator"
dctermsMedium :: URI
dctermsMedium = fromJust $ parseURI "http://purl.org/dc/terms/medium"
dctermsModified :: URI
dctermsModified = fromJust $ parseURI "http://purl.org/dc/terms/modified"
dctermsProvenance :: URI
dctermsProvenance = fromJust $ parseURI "http://purl.org/dc/terms/provenance"
dctermsPublisher :: URI
dctermsPublisher = fromJust $ parseURI "http://purl.org/dc/terms/publisher"
dctermsReferences :: URI
dctermsReferences = fromJust $ parseURI "http://purl.org/dc/terms/references"
dctermsRelation :: URI
dctermsRelation = fromJust $ parseURI "http://purl.org/dc/terms/relation"
dctermsReplaces :: URI
dctermsReplaces = fromJust $ parseURI "http://purl.org/dc/terms/replaces"
dctermsRequires :: URI
dctermsRequires = fromJust $ parseURI "http://purl.org/dc/terms/requires"
dctermsRights :: URI
dctermsRights = fromJust $ parseURI "http://purl.org/dc/terms/rights"
dctermsRightsHolder :: URI
dctermsRightsHolder = fromJust $ parseURI "http://purl.org/dc/terms/rightsHolder"
dctermsSource :: URI
dctermsSource = fromJust $ parseURI "http://purl.org/dc/terms/source"
dctermsSpatial :: URI
dctermsSpatial = fromJust $ parseURI "http://purl.org/dc/terms/spatial"
dctermsSubject :: URI
dctermsSubject = fromJust $ parseURI "http://purl.org/dc/terms/subject"
dctermsTableOfContents :: URI
dctermsTableOfContents = fromJust $ parseURI "http://purl.org/dc/terms/tableOfContents"
dctermsTemporal :: URI
dctermsTemporal = fromJust $ parseURI "http://purl.org/dc/terms/temporal"
dctermsTitle :: URI
dctermsTitle = fromJust $ parseURI "http://purl.org/dc/terms/title"
dctermsType :: URI
dctermsType = fromJust $ parseURI "http://purl.org/dc/terms/type"
dctermsValid :: URI
dctermsValid = fromJust $ parseURI "http://purl.org/dc/terms/valid"
